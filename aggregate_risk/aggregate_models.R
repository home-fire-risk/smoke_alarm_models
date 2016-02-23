## This script generates the weighted indicator of "risk" to plot for each census tract that will be used for the map visualization.
## This data will be stored in /data/risk_tract.csv.


## libraries
library('data.table')
library('bit64') # per warning in data.table from model_1b output
library('readxl') 
library('UScensus2010')
#library('UScensus2010tract') # not sure if this is needed

dt1a <- fread('model_1a_RC_homevisit/results/smoke-alarm-risk-scores.csv') # still working creating this scores
dt1b <- fread('model_1b_nfirs_smokealarm_pres/Output/tracts_74k_weighted_linear_preds_upsampled.csv', colClasses=c('tractid'='character'))
dt1c <- fread('model_1c_enigma_ahs_smokealarm/results/smoke-alarm-risk-scores.csv')
dt2a <- fread('model_2c_ind_RC_response/summary_output/fires_per_tract.csv', colClasses=c('x'='character'))
dt3a <- fread('model_3a_casualty_given_fire/results/results_tract.csv')

# functions used to normalize risk scores
normalize <- function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T)- min(x, na.rm=T))
balscale <- function(x, w_logit=0.75, w_normalized=0.25) plogis(scale(x))*w_logit + normalize(x)*w_normalized

#######################################
## Processing results from Model 1.A ##
#######################################

dt1a[, tract_geoid:=paste0(state, cnty, tract)]
dt1a[, risk_1a:=balscale(risk_1a)*100]

#######################################
## Processing results from Model 1.B ##
#######################################

setnames(dt1b, 'tractid', 'tract_geoid')
dt1b[nchar(tract_geoid)==10, tract_geoid:=paste0('0', tract_geoid)] # adding leading 0 to tract_geoid

# creating normalized risk score
dt1b[, risk_1b := balscale(weighted_linear_pred)*100]

#######################################
## Processing results from Model 1.C ##
#######################################

## separating out geographies. block from census tract
dt1c[,bg_geoid:=as.character(bg_geoid)]

dt1c[,state:=substr(bg_geoid, 1,2)]
dt1c[,cnty:=substr(bg_geoid, 3,5)]
dt1c[,tract:=substr(bg_geoid, 6,11)]
dt1c[,block:=substr(bg_geoid, 12,12)]

## calculating census tract-level risk.
dt1c_tract <- dt1c[,.(
  risk_1c=mean(smoke_alarm_risk)*100,
  tract_pop=sum(bg_pop),
  blocks_per_tract=.N
), by=.(state, cnty, tract)]

dt1c_tract[,risk_1c := balscale(risk_1c)*100]

dt1c_tract[, tract_geoid:=paste0(state, cnty, tract)]


#######################################
## Processing results from Model 2.A ##
#######################################

dt2a[, V1:=NULL]
setnames(dt2a, 'x', 'tract_geoid')

# correcting geoid by getting back leading 0s
dt2a[nchar(tract_geoid)=='10', tract_geoid:=paste0('0', tract_geoid)] 

## getting population per tract
dtpopL <- mapply(function(st) demographics(dem='P0010001', level='tract', state=st), st=c(state.abb, 'DC')) # 38 seconds
dtpop <- rbindlist(lapply(dtpopL, function(x) as.data.table(x, keep.rownames=T)))
setnames(dtpop, 'rn', 'tract_geoid')
rm(dtpopL) # cleanup
rm(list=grep('\\.tract10', ls(), value=T)) # clean up

## creating indicator per capita
dt2a <- merge(dt2a, dtpop, by='tract_geoid', all=T)
dt2a[is.na(freq), freq:=0]
dt2a[is.na(P0010001), P0010001:=0]
dt2a[,firesPer1000:=freq/(P0010001+100)*1000]

dt2a[,firesPer1000_scaled:=firesPer1000*log(P0010001)] # 
dt2a[,risk_2a:=balscale(firesPer1000_scaled)]  ## why is this not working?


#######################################
## Processing results from Model 3.A ##
#######################################

# probabilities are low, so forcing normalization of scores to be between 0 and 1.  this could be improved later per feedback from ARC
dt3a[, risk_3a := balscale(risk_3a)*100]
setnames(dt3a, 'geoid', 'tract_geoid')

#######################################
## AGGREGATING RESULTS ################
#######################################

# setting key for merge
setkey(dt1a, tract_geoid)
setkey(dt1b, tract_geoid)
setkey(dt1c_tract, tract_geoid)
setkey(dt3a, tract_geoid)

# merging risk scores together
dt <- merge(dt1a[,.(tract_geoid, risk_1a)], dt1b, all=T)
dt <- merge(dt, dt1c_tract[,.(tract_geoid, risk_1c, cnty, state, tract, tract_pop)], all=T)
dt <- merge(dt, dt3a[,.(tract_geoid, risk_3a)], all=T)

# calculating aggregated risk score.  simple average now. will be built into function with ability to weight each indicator differently.
dt$risk <- rowMeans(dt[,.(risk_1a, risk_1b, risk_1c, risk_3a)], na.rm=T)

#Creating County Average 
dt[, risk_cnty:=mean(risk), by=.(state, cnty)]

#######################################
## MERGING ARC REGIONS ################
#######################################

# reading in crosswalk ARC region <=> FIPS data
fileurl <- 'http://maps.redcross.org/website/Services/Data/2015_Chapter_Alignment_Master_No_Contacts.xlsx'
download.file(fileurl, '2015_Chapter_Alignment_Master_No_Contacts.xlsx')
reg <- data.table(read_excel('2015_Chapter_Alignment_Master_No_Contacts.xlsx', sheet = 'COUNTY_CHAP_NO_CONTACTS', skip = 1))
setnames(reg, names(reg), tolower(gsub(' ', '_', names(reg))))

# deduping
reg <- reg[,head(.SD, 1), by='county_fips']

# cross-walking
dt[, county_fips:=substr(tract_geoid, 0, 5)]
dt <- merge(dt, reg[,.(county_fips, region_code, region_name, chapter_code, chapter_name, county_name_long)], by='county_fips', all.x=T, all.y=F)

# cleaing up
file.remove('2015_Chapter_Alignment_Master_No_Contacts.xlsx') # remove file after merge, no need to keep clutter

#######################################
## WRITING OUT RESULTS ################
#######################################

write.table(dt[,.(state, cnty, tract, tract_geoid, region_code, region_name, chapter_code, chapter_name, county_name_long, risk_cnty, risk, risk_1a, risk_1b, risk_1c, risk_3a, tract_pop)], file='aggregate_risk/data/risk_tract.csv', sep=',', row.names=F)

