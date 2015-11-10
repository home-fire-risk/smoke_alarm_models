#' Loop over data, isolating sequential chunks, merge data as appropriate,
#' then scoring
#' 
rm(list = ls())
gc()


library(data.table)
library(magrittr)
#library(caret)
library(DMwR)
library(randomForest)

#######################
# Load base data sets #
#######################

load("data/rdata/inc.rdata")
setkey(inc, geoid)
inc = inc[!is.na(geoid)]
inc[,target:="no_inj"]
inc[oth_inj>0, target:="inj"]
inc[oth_death>0, target:="death"]

class_prior = inc[,mean(target!="no_inj")]

load("data/rdata/tract_data.Rdata") # sl - 74101 obs, 270 vars
sl = data.table(sl)
setnames(sl, names(sl), tolower(names(sl)))
sl = sl[!is.na(geoid)]

drop_vars = c("county_fip", "county_fip_full",
              #"geoid", # need this for merge
              "state", "statecountyfipscode", "tract_fip",
              "gov",
              "geoid",
              grep("^base",names(sl), value=TRUE),
              grep("^member",names(sl), value=TRUE),
              grep("^select",names(sl), value=TRUE)
)

drop_ix = which(names(sl) %in% drop_vars)
sl2 = sl[,-drop_ix, with=FALSE]

############################################
# Handle characters that should be numeric #
############################################

sl2[,populationdensitypersquaremile20:=gsub(",","",populationdensitypersquaremile20)]
sl2[,populationdensitypersquaremile20:=gsub('"','',populationdensitypersquaremile20)]
sl2[populationdensitypersquaremile20==".", populationdensitypersquaremile20:=NA]
sl2[,populationdensitypersquaremile20:=as.numeric(populationdensitypersquaremile20)]

sl2[,tractpopulation2010:=gsub(",","",tractpopulation2010)]
sl2[,tractpopulation2010:=gsub('"','',tractpopulation2010)]
sl2[,tractpopulation2010:=as.numeric(tractpopulation2010)]

sl2[,landareasquaremiles2010:=gsub(",","",landareasquaremiles2010)]
sl2[,landareasquaremiles2010:=gsub('"','',landareasquaremiles2010)]
sl2[,landareasquaremiles2010:=as.numeric(landareasquaremiles2010)]

#######################
# Handle missing data #
#######################

system.time(
  sl2 <- centralImputation(sl2) # 1.97s elapsed
)

###############
# Load models #
###############

load("models/mod_casualty_rf_prototype.Rdata")

####################
# calculate scores #
####################

system.time(
  ypred <- predict(mod_casualty_rf, sl2, type="prob") # 7.29s
)

dim(ypred)
dim(sl)
head(ypred)

sl$prob_death_or_inj = ypred[,1] 
sl$prob_death_or_inj_adj = ypred[,1] * (class_prior)/.5

hist(sl$prob_death_or_inj)
hist(sl$prob_death_or_inj_adj)

#######################################
# Persist scores associated to geoids #
#######################################

sl$risk_3a = sl$prob_death_or_inj_adj
write.csv(sl[,.(geoid, risk_3a)], file="results/results_tract.csv", row.names=FALSE)

# Don't really need this recipe for modeling, but will need it for training.
# The overhead comes from the join on geoid between incidents and tracts.
# the combination of high cardinality on incidents and high dimensionality on
# tracts is problematic. Scoring is just at the tract level, so there shouldn't
# be a problem scoring the whole dataset. *Evaluating* the scores relative to
# historical incidents, on the other hand, is a bit more complicated. Let's 
# deal with that later.
if(FALSE){
  ##########################
  # Process chunks of data #
  ##########################
  
  chunksize = 50000
  N = nrow(sl)
  start_ix = 1
  while(start_ix<=N){
    end_ix   = start_ix + chunksize - 1
    if(end_ix>N){
      end_ix=N
    }
    
    recs = sl[start_ix:end_ix]
    ypred = predict(mod_casualty_rf, recs)
    break
  }
}
