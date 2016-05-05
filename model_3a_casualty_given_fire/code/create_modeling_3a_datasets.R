library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)
library(caret)
library(DMwR)
library(randomForest)
library(foreign)
library(bit64)


####################################################################################
# Creating the modeling dataset by linking NFIRS and ACS data
####################################################################################

rm(list = ls())

years = as.character(seq(2009, 2013))
downloads_path = '/users/nickbecker/downloads/'


for(year in years) {
  cat(year)
  
  ### Read in the data
  data_formatted = fread(paste0(downloads_path, year, "/", year, "_formated_addresses.csv"), data.table = FALSE)
  data_geocoded = fread(paste0(downloads_path, year, "/", year, "_geocoded_addresses.csv"), data.table = FALSE)
  
  # remove extra 1st column
  data_formatted = data_formatted[, -1]
  data_geocoded = data_geocoded[, -1]
  
  ### Bring the geocoded information into the original data and keep the relevant information
  data_combined = data_formatted %>%
    left_join(data_geocoded, by = "row_seq") %>%
    select(STATE:EXP_NO, tabblock_id:tractid) %>%
    mutate(FDID = as.character(FDID),
           INC_NO = as.character(INC_NO))
  
  rm(list = c("data_formatted", "data_geocoded")) # memory management
  
  ### Bring in the NFIRS data on outcomes
  
  if(year %in% c("2012", "2013")) {
    data_combined = data_combined %>%
      mutate(INC_DATE = as.integer(INC_DATE))
  }
  
  if(year %in% c("2009", "2010", "2011")) {
    nfirs_incident_data = read.dbf(paste0(downloads_path, "NFIRS_", year, "/basicincident.dbf"))
  }
  
  else if(year %in% c("2012", "2013")) {
    nfirs_incident_data = fread(paste0(downloads_path, "NFIRS_", year, "/basicincident.txt"), sep = "^",
                              data.table = FALSE)
  }
  
  nfirs_incident_data_relevant = nfirs_incident_data %>%
    select(STATE:EXP_NO, FF_DEATH:OTH_INJ)
  
  rm(list = c("nfirs_incident_data"))
  
  ### Join the census tracts to the NFIRS data
  cat("Merging tracts to NFIRS now")
  
  nfirs_outcomes_geo = nfirs_incident_data_relevant %>%
    left_join(data_combined, by = c("STATE", "FDID", "INC_DATE", "INC_NO", "EXP_NO"))
  
  rm(list = c("data_combined"))
  rm(list = c("nfirs_incident_data_relevant"))
  
  ### Clean the outcomes data up a bit
  nfirs_outcomes_geo_clean = nfirs_outcomes_geo %>%
    filter(!is.na(tractid)) %>%
    filter(tractid != "") %>%
    group_by(STATE, FDID, INC_DATE, INC_NO, EXP_NO) %>%
    mutate(count_in_group = row_number()) %>%
    filter(count_in_group == 1) %>%
    select(-count_in_group) %>%
    as.data.frame()
  
  rm(list = c("nfirs_outcomes_geo"))
  
  ### Save cleaned data
  save(nfirs_outcomes_geo_clean, file = paste0("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_", year, ".Rdata"))
  rm(list = c("nfirs_outcomes_geo_clean"))
  
  Sys.sleep(20)
  
}

# 
# data_formatted = fread(paste0(downloads_path, 2009, "/", 2009, "_formated_addresses.csv"))
# data_geocoded = fread(paste0(downloads_path, 2009, "/", 2009, "_geocoded_addresses.csv"))
# 
# data_formatted = read_csv(paste0(downloads_path, 2010, "/", 2010, "_formated_addresses.csv"))
# data_geocoded = read_csv(paste0(downloads_path, 2010, "/", 2010, "_geocoded_addresses.csv"))
# 
# data_formatted = read_csv(paste0(downloads_path, 2011, "/", 2011, "_formated_addresses.csv"))
# data_geocoded = read_csv(paste0(downloads_path, 2011, "/", 2011, "_geocoded_addresses.csv"))
# 
# data_formatted = read_csv(paste0(downloads_path, 2012, "/", 2012, "_formated_addresses.csv"))
# data_geocoded = read_csv(paste0(downloads_path, 2012, "/", 2012, "_geocoded_addresses.csv"))
# 
# 
# 
# load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_2009.Rdata")
# 

# Something's weird with the 2009 and 2012 data. The merge only assigns around 200 tract IDs
# For now, ignore.
# remove extra 1st column

#  
# data_formatted = data_formatted[, -1]
# data_geocoded = data_geocoded[, -1]
# 
# ### Bring the geocoded information into the original data and keep the relevant information
# data_combined = data_formatted %>%
#   left_join(data_geocoded, by = "row_seq") %>%
#   select(STATE:EXP_NO, tabblock_id:tractid) %>%
#   mutate(FDID = as.character(FDID),
#          INC_NO = as.character(INC_NO))
# 
# data_combined = filter(data_combined, !is.na(tractid))
# 
# summary(data_combined$tractid)
# sum(data_combined$tractid == "")
# 
# glimpse(data_combined)
# 
# nrow(filter(data_combined, tractid == ""))
# 
# rm(list=ls())



### Load and combine multiple years of data
load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_2009.Rdata")
nfirs_outcomes_geo_clean_2009 = nfirs_outcomes_geo_clean

rm("nfirs_outcomes_geo_clean")

load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_2010.Rdata")
nfirs_outcomes_geo_clean_2010 = nfirs_outcomes_geo_clean

rm("nfirs_outcomes_geo_clean")

load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_2011.Rdata")
nfirs_outcomes_geo_clean_2011 = nfirs_outcomes_geo_clean

rm("nfirs_outcomes_geo_clean")

nfirs_outcomes_geo_clean_multi_year = bind_rows(nfirs_outcomes_geo_clean_2009, nfirs_outcomes_geo_clean_2010,
                                                nfirs_outcomes_geo_clean_2011)

rm("nfirs_outcomes_geo_clean_2009", "nfirs_outcomes_geo_clean_2010", "nfirs_outcomes_geo_clean_2011")

load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_2012.Rdata")
nfirs_outcomes_geo_clean_2012 = nfirs_outcomes_geo_clean

rm("nfirs_outcomes_geo_clean")

nfirs_outcomes_geo_clean_multi_year = bind_rows(nfirs_outcomes_geo_clean_multi_year, nfirs_outcomes_geo_clean_2012)

rm("nfirs_outcomes_geo_clean_2012")

load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_2013.Rdata")
nfirs_outcomes_geo_clean_2013 = nfirs_outcomes_geo_clean

rm("nfirs_outcomes_geo_clean")

nfirs_outcomes_geo_clean_multi_year = bind_rows(nfirs_outcomes_geo_clean_multi_year, nfirs_outcomes_geo_clean_2013)

rm("nfirs_outcomes_geo_clean_2013")

save(nfirs_outcomes_geo_clean_multi_year, file = "/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_multi_year.Rdata")
rm("nfirs_outcomes_geo_clean_multi_year")


Sys.sleep(20)



















####################################################################################
# Creating the modeling dataset for predicting death vs. other
####################################################################################

rm(list=ls())
load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_multi_year.Rdata")

nrow(filter(nfirs_outcomes_geo_clean_multi_year, tractid == ""))

### Create target variable
nfirs_outcomes_geo_clean_multi_year = nfirs_outcomes_geo_clean_multi_year %>%
  filter(tractid != "") %>%
  mutate(target = "other") %>%
  mutate(target = replace(target, OTH_DEATH >= 1, "death"))

glimpse(nfirs_outcomes_geo_clean_multi_year)

table(nfirs_outcomes_geo_clean_multi_year$target)/nrow(nfirs_outcomes_geo_clean_multi_year)


### Let's unbalanced downsample
source("/Users/nickbecker/Documents/Github/Superseded/arc_smoke_alarm_old_2/models/model_3a_casualty_given_fire/code/general_purpose/unbalanced_downsample.r")

ix = unbalanced_downsample(nfirs_outcomes_geo_clean_multi_year$target, 
                           c("death","other"), 
                           c(.5, .5),
                           n = 20000) %>%
  unlist


nfirs_outcomes_geo_clean_multi_year_unbalanced = nfirs_outcomes_geo_clean_multi_year[ix,]

table(nfirs_outcomes_geo_clean_multi_year$target)
table(nfirs_outcomes_geo_clean_multi_year_unbalanced$target)



### Bring in the relevant ACS features
acs_tract_data = read_csv("/users/nickbecker/Downloads/acs_tract_data.csv")
glimpse(acs_tract_data)

nfirs_outcomes_geo_clean_multi_year_unbalanced = nfirs_outcomes_geo_clean_multi_year_unbalanced %>%
  mutate(tractid = as.numeric(tractid))

nfirs_outcomes_geo_clean_multi_year_acs = nfirs_outcomes_geo_clean_multi_year_unbalanced %>%
  left_join(acs_tract_data, by = "tractid") %>%
  filter(!is.na(tractid))

glimpse(nfirs_outcomes_geo_clean_multi_year_acs)
summary(nfirs_outcomes_geo_clean_multi_year_acs)


### Remove the merging variables, keeping just census data and target
modeling_death_dataset = nfirs_outcomes_geo_clean_multi_year_acs %>%
  select(-(STATE:county)) %>%
  #na.omit() %>% # optional. could do imputation instead
  mutate(target = as.factor(target)) %>%
  as.data.frame()


system.time(
  modeling_death_dataset_imp <- centralImputation(modeling_death_dataset)
)

glimpse(modeling_death_dataset_imp)


write.csv(modeling_death_dataset_imp, "/users/nickbecker/Documents/R Workspace/modeling_death_dataset_imp.csv",
          row.names = FALSE)






####################################################################################
# Creating the modeling dataset for predicting injury vs. other
####################################################################################

rm(list=ls())
load("/users/nickbecker/Documents/R Workspace/nfirs_outcomes_geo_clean_multi_year.Rdata")

### Create target variable
nfirs_outcomes_geo_clean_multi_year = nfirs_outcomes_geo_clean_multi_year %>%
  filter(tractid != "") %>%
  mutate(target = "other") %>%
  mutate(target = replace(target, OTH_INJ >= 1, "injury"))

glimpse(nfirs_outcomes_geo_clean_multi_year)

table(nfirs_outcomes_geo_clean_multi_year$target)/nrow(nfirs_outcomes_geo_clean_multi_year)


### Let's unbalanced downsample
source("/Users/nickbecker/Documents/Github/Superseded/arc_smoke_alarm_old_2/models/model_3a_casualty_given_fire/code/general_purpose/unbalanced_downsample.r")

ix = unbalanced_downsample(nfirs_outcomes_geo_clean_multi_year$target, 
                           c("injury","other"), 
                           c(.5, .5),
                           n = 20000) %>%
  unlist


nfirs_outcomes_geo_clean_multi_year_unbalanced = nfirs_outcomes_geo_clean_multi_year[ix,]

table(nfirs_outcomes_geo_clean_multi_year$target)
table(nfirs_outcomes_geo_clean_multi_year_unbalanced$target)



### Bring in the relevant ACS features
acs_tract_data = read_csv("/users/nickbecker/Downloads/acs_tract_data.csv")
glimpse(acs_tract_data)

nfirs_outcomes_geo_clean_multi_year_unbalanced = nfirs_outcomes_geo_clean_multi_year_unbalanced %>%
  mutate(tractid = as.numeric(tractid))

nfirs_outcomes_geo_clean_multi_year_acs = nfirs_outcomes_geo_clean_multi_year_unbalanced %>%
  left_join(acs_tract_data, by = "tractid") %>%
  filter(!is.na(tractid))

glimpse(nfirs_outcomes_geo_clean_multi_year_acs)
#summary(nfirs_outcomes_geo_clean_multi_year_acs)


### Remove the merging variables, keeping just census data and target
modeling_injury_dataset = nfirs_outcomes_geo_clean_multi_year_acs %>%
  select(-(STATE:county)) %>%
  #na.omit() %>% # optional. could do imputation instead
  mutate(target = as.factor(target)) %>%
  as.data.frame()


system.time(
  modeling_injury_dataset_imp <- centralImputation(modeling_injury_dataset)
)

glimpse(modeling_injury_dataset_imp)


write.csv(modeling_injury_dataset_imp, "/users/nickbecker/Documents/R Workspace/modeling_injury_dataset_imp.csv",
          row.names = FALSE)


