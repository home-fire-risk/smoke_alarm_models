## This script generates the weighted indicator of "risk" to plot for each census tract that will be used for the map visualization.
## This data will be stored in /data/risk_tract.csv.


## libraries
library('data.table')

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

## Writing out results
write.table(dt[,.(state, cnty, tract, tract_geoid, risk, risk_1a, risk_1c, tract_pop)], file='aggregate_risk/data/risk_tract.csv', sep=',', row.names=F)

