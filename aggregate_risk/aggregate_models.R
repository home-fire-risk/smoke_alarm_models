## This script generates the weighted indicator of "risk" to plot for each census tract that will be used for the map visualization.
## This data will be stored in /data/risk_tract.csv.


## libraries
library('data.table')
library('readxl') 

dt1a <- fread('model_1a_RC_homevisit/results/smoke-alarm-risk-scores.csv') # still working creating this scores
dt1c <- fread('model_1c_enigma_ahs_smokealarm/results/smoke-alarm-risk-scores.csv')
dt3a <- fread('model_3a_casualty_given_fire/results/results_tract.csv')

#######################################
## Processing results from Model 1.A ##
#######################################

dt1a[, tract_geoid:=paste0(state, cnty, tract)]
dt1a[, risk_1a:=risk_1a*100]


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

dt1c_tract[, tract_geoid:=paste0(state, cnty, tract)]


#######################################
## Processing results from Model 3.A ##
#######################################

# probabilities are low, so forcing normalization of scores to be between 0 and 1.  this could be improved later per feedback from ARC
dt3a[, risk_3a := (risk_3a - min(risk_3a))/(max(risk_3a) - min(risk_3a))]
setnames(dt3a, 'geoid', 'tract_geoid')

#######################################
## AGGREGATING RESULTS ################
#######################################

# setting key for merge
setkey(dt1a, tract_geoid)
setkey(dt1c_tract, tract_geoid)
setkey(dt3a, tract_geoid)

# merging risk scores together
dt <- merge(dt1a[,.(tract_geoid, risk_1a)], dt1c_tract, all=T)
dt <- merge(dt, dt3a[,.(tract_geoid, risk_3a)], all=T)

# calculating aggregated risk score.  simple average now. will be built into function with ability to weight each indicator differently.
dt$risk <- rowMeans(dt[,.(risk_1a, risk_1c, risk_3a)], na.rm=T)

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

write.table(dt[,.(state, cnty, tract, tract_geoid, region_code, region_name, chapter_code, chapter_name, county_name_long, risk_cnty, risk, risk_1a, risk_1c, tract_pop)], file='aggregate_risk/data/risk_tract.csv', sep=',', row.names=F)

