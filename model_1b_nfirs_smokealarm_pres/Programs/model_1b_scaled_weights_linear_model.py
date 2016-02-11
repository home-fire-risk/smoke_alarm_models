# -*- coding: utf-8 -*-
"""
Created on Tue Feb  9 13:23:30 2016

@author: nickbecker
"""


import pandas as pd
import numpy as np
import scipy
from sklearn import linear_model
from sklearn.cross_validation import train_test_split, cross_val_score
from sklearn.preprocessing import normalize, Imputer
from sklearn.metrics import mean_squared_error
from math import sqrt


acs = pd.read_csv('/users/nickbecker/Downloads/acs.csv')

tract_data = pd.read_csv('/users/nickbecker/Downloads/2009_2013_alarm_presence_by_tract.csv')

del tract_data['Unnamed: 0']

acs.columns

# Get the tract id from the geoid
x = acs['geoid'][100]
parsed_id = x.split('US')[1]
sum_level = x.split('US')[0]



acs['geoid_parsed'] = [geo[1] for geo in acs['geoid'].str.split('US')]
acs['sum_level'] = [geo[0] for geo in acs['geoid'].str.split('US')]

# understanding census coding: https://www.census.gov/geo/reference/geoidentifiers.html
# sumlevel == 15000 is block group
# sumlevel == 14000 is census tract
# sumlevel == 05000 is county

# pulling out state and county for ease of use
#acs['state'] = acs['geoid_parsed'].str[0:2]
#acs['cnty'] = acs['geoid_parsed'].str[2:5]
#acs['raw_tract'] = acs['geoid_parsed'].str[5:]

#acsC = acs.query("sum_level == '05000' ")
acsCT = acs.query("sum_level == '14000'")

acsCT['geoid'].tail()
#acsCT['state'].tail()
#acsCT['cnty'].tail()
#acsCT['raw_tract'].tail()
acsCT['geoid_parsed'].tail()
# Only need to keep the tract level, so clear some space in memory
#del acs


########################################
# MERGING PREDICTORS & TARGET VARIABLE:
########################################


acsCT = acsCT.rename(columns = {'geoid_parsed':'tractid'})
acsCT['tractid'] = acsCT['tractid'].astype(int)
acs_tractid = acsCT['tractid']

acsCT.query(" tractid == 56043000200 ")
acsCT = acsCT.drop(['sum_level', 'geoid'], axis = 1)

#acsCT.dropna()



########################################################
# Handle missing observations by simple mean imputation
acs_features = acsCT.drop(['tractid'], axis = 1)

col_names = acs_features.columns

imp = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0).fit(acs_features)
acs_features = imp.transform(acs_features)

acsCT = pd.DataFrame(acs_features)
acsCT.columns = col_names
acsCT = pd.concat([acsCT.reset_index(drop=True), acs_tractid.reset_index(drop=True)], axis = 1)
########################################################



tract_data['tractid'].tail()
tract_data_merged = tract_data.merge(acsCT, how='left', on='tractid')
#tract_data_merged.tail()

tract_data_merged.isnull().sum()


#tract_data_merged_filtered = tract_data_merged.query(" all_fire_all_years > 25 ").dropna()
tract_data_merged_filtered = tract_data_merged.query(" all_fire_all_years > 25 ")

#for x in tract_data_merged_filtered.columns:
#    print(x)

tract_data_merged_filtered.isnull().sum()


'''
#### upsample at arbitrary threshold for testing
from sklearn.utils import resample
high_fires_resampled = resample(tract_data_merged_filtered.query( "all_fire_all_years >= 50" ), n_samples = 1000, random_state = 12)

tract_data_merged_filtered = tract_data_merged_filtered.append(high_fires_resampled)
'''




########################################
# Model Preparation
########################################


data_features = tract_data_merged_filtered.drop('ratio_no_alarm_in_all', axis = 1)
data_features = data_features.drop(['tractid', 'alarm_unknown_all_years',
                                    'alarm_presented_all_years', 'alarm_not_presented_all_years',
                                    'ratio_no_alarm_in_all_known'], axis = 1)
data_target = tract_data_merged_filtered['ratio_no_alarm_in_all']

data_features.isnull().sum()


# Handle missing observations by simple mean imputation
col_names = data_features.columns
imp = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0).fit(data_features)
data_features = imp.transform(data_features)

data_features = pd.DataFrame(data_features)
data_features.columns = col_names

data_features.isnull().sum()


#observation_weights = data_features.all_fire_all_years/data_features.all_fire_all_years.sum()
#observation_weights_normalized = normalize(observation_weights, norm = 'l1')
#data_features = data_features.drop(['all_fire_all_years'], axis = 1)

x_train, x_test, y_train, y_test = train_test_split(data_features,
                                                    data_target,
                                                    test_size=0.1,
                                                    random_state=12)

all_fires = x_train.all_fire_all_years

observation_weights = x_train.all_fire_all_years/x_train.all_fire_all_years.sum()
observation_weights_normalized = normalize(observation_weights, norm = 'l1').ravel()
scaled_weights = x_train.all_fire_all_years/np.max(x_train.all_fire_all_years)

x_train = x_train.drop(['all_fire_all_years'], axis = 1)
x_test = x_test.drop(['all_fire_all_years'], axis = 1)



# Output to csv
x_train.to_csv('/users/nickbecker/Downloads/x_train.csv', header = True, index = False)
x_test.to_csv('/users/nickbecker/Downloads/x_test.csv', header = True, index = False)
y_train.to_csv('/users/nickbecker/Downloads/y_train.csv', header = True, index = False)
y_test.to_csv('/users/nickbecker/Downloads/y_test.csv', header = True, index = False)
all_fires.to_csv('/users/nickbecker/Downloads/all_fires.csv', header = True, index = False)



########################################
# MODELING
########################################


clf_linear = linear_model.SGDRegressor(loss = 'squared_loss', penalty = 'none', random_state = 12)
clf_linear.fit(x_train, y_train, sample_weight=scaled_weights)
#clf_linear.fit(x_train, y_train, sample_weight=observation_weights_normalized)
#clf_linear.fit(x_train, y_train)
#clf_linear = linear_model.LogisticRegression()
#clf_linear.fit(x_train, y_train)

y_pred = clf_linear.predict(x_test)

# RMSE of simple linear model
rmse = sqrt(mean_squared_error(y_test, y_pred))
print rmse




#### Use model to predict for every census tract in the geocoded data (68075) ####

# Clean up original full dataset
tract_data_merged_simple = tract_data_merged.drop(['tractid', 'alarm_unknown_all_years',
                                    'alarm_presented_all_years', 'alarm_not_presented_all_years',
                                    'ratio_no_alarm_in_all_known', 'ratio_no_alarm_in_all', 'all_fire_all_years'], axis = 1)


#tract_data_merged_simple_clean = tract_data_merged.dropna()
#tract_data_merged_simple = tract_data_merged_simple_clean.drop(['tractid', 'sum_level', 'geoid', 'alarm_unknown_all_years',
#                                    'alarm_presented_all_years', 'alarm_not_presented_all_years',
#                                    'ratio_no_alarm_in_all_known', 'ratio_no_alarm_in_all', 'all_fire_all_years'], axis = 1)

                               

tract_data_merged_simple.isnull().sum()

# Handle missing observations by simple mean imputation
col_names = tract_data_merged_simple.columns
imp = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0).fit(tract_data_merged_simple)
tract_data_merged_simple = imp.transform(tract_data_merged_simple)

tract_data_merged_simple = pd.DataFrame(tract_data_merged_simple)
tract_data_merged_simple.columns = col_names                                    
                  
tract_data_merged_simple.isnull().sum()
                  
full_preds = clf_linear.predict(tract_data_merged_simple)

### Bound probabilities
#full_preds[full_preds < 0] = 0
#full_preds[full_preds > 1] = 1

# RMSE of simple linear model on the full dataset
#print sqrt(mean_squared_error(tract_data_merged_simple_clean.ratio_no_alarm_in_all, full_preds))
print sqrt(mean_squared_error(tract_data_merged.ratio_no_alarm_in_all, full_preds))



#tract_data['weighted_linear_pred'] = full_preds
tract_data['weighted_linear_pred'] = full_preds
#tract_data_merged['weighted_linear_pred'] = full_preds

tract_data = tract_data.sort(['all_fire_all_years'], ascending = False)


tract_data['weighted_linear_pred'].describe()
print tract_data['weighted_linear_pred'].tail()

# Output to csv
tract_data.to_csv('/users/nickbecker/Downloads/tract_data_weighted_linear_preds_upsampled.csv', header = True, index = False)










#### Use model to predict for every census tract in the census data (74,001) ####

full_preds_acsCT = clf_linear.predict(acs_features)

### Bound probabilities
#full_preds[full_preds < 0] = 0
#full_preds[full_preds > 1] = 1


# Can't compute RMSE because we have no target for some of the tracts.

tract_preds_74k = pd.concat([acs_tractid.reset_index(drop=True), pd.Series(full_preds_acsCT)], axis = 1)

tract_preds_74k.columns = ['tractid', 'weighted_linear_pred']

tract_preds_74k.weighted_linear_pred.describe()


# Output to csv
tract_preds_74k.to_csv('/users/nickbecker/Downloads/tracts_74k_weighted_linear_preds_upsampled.csv', header = True, index = False)












