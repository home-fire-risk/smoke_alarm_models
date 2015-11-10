#' Clear and build workspace
rm(list = ls())
gc()


library(data.table)
library(magrittr)
source("code/general_purpose/unbalanced_downsample.r")

library(caret)
library(DMwR)
library(randomForest)


load("data/rdata/inc.rdata")
setkey(inc, geoid)
inc = inc[!is.na(geoid)]
inc[,target:="no_inj"]
inc[oth_inj>0, target:="inj"]
inc[oth_death>0, target:="death"]

if(FALSE){
  inc[,geoid2:=geoid]
  head(inc[,.(geoid, geoid2)])
  #head(inc[ix,.(geoid, geoid2)])
  
  table(inc$target)
  
  # death     inj  no_inj 
  # 10849   56890 4058912 
  
  table(inc$target)/nrow(inc)
  # death         inj      no_inj 
  # 0.002629008 0.013785997 0.983584994 
}

load("data/rdata/tract_data.Rdata") # sl - 74101 obs, 270 vars
sl = data.table(sl)
setnames(sl, names(sl), tolower(names(sl)))
sl = sl[!is.na(geoid)]


#############################################
# Let's develop a single modeling iteration #
#############################################

ix = unbalanced_downsample(inc[,target], 
                           c("death","inj","no_inj"), 
                           c(.25, .25, .50)) %>%
       unlist
#sapply(ix, length)
#length(ix) # number of records in a single modeling iteration: 43424
#table(inc[ix, target])

#head(inc[,.(geoid, geoid2)])
#head(inc[ix,.(geoid, geoid2)])

table(inc$target)

inc = inc[ix]

drop_vars = c("county_fip", "county_fip_full",
              #"geoid", # need this for merge
              "state", "statecountyfipscode", "tract_fip",
              "gov",
              grep("^base",names(sl), value=TRUE),
              grep("^member",names(sl), value=TRUE),
              grep("^select",names(sl), value=TRUE)
              )
  ### "inc_date", # maybe turn this into month or something

#sort(names(sl))

# Extract relevant records, merge on sl to construct training dataset

drop_ix = which(names(sl) %in% drop_vars)
sl_train = sl[,-drop_ix, with=FALSE]

setkey(sl_train,geoid)
setkey(inc, geoid)
train = sl_train[inc]

#dim(train) # 43396   219

# After merging, drop records we weren't able to merge (bad geoid alignment)
#train %>% names %>% sort
train[is.na(civic_obama), .N] # 97
train = train[!is.na(civic_obama)]
#head(train)


drop_vars2 = c(setdiff(names(inc), "target"),
               grep("i\\.", names(train), value=TRUE)
               )
drop_ix2 = which(names(train) %in% drop_vars2)

train = train[,-drop_ix2, with=FALSE]
#head(train)

### Need to handle numeric columns that are still registering as 
### strings due to comma division of 10^3 places.
###############################
train[,populationdensitypersquaremile20:=gsub(",","",populationdensitypersquaremile20)]
train[,populationdensitypersquaremile20:=gsub('"','',populationdensitypersquaremile20)]
sl2[populationdensitypersquaremile20==".", populationdensitypersquaremile20:=NA]
train[,populationdensitypersquaremile20:=as.numeric(populationdensitypersquaremile20)]

train[,tractpopulation2010:=gsub(",","",tractpopulation2010)]
train[,tractpopulation2010:=gsub('"','',tractpopulation2010)]
train[,tractpopulation2010:=as.numeric(tractpopulation2010)]

train[,landareasquaremiles2010:=gsub(",","",landareasquaremiles2010)]
train[,landareasquaremiles2010:=gsub('"','',landareasquaremiles2010)]
train[,landareasquaremiles2010:=as.numeric(landareasquaremiles2010)]


#train %>% names %>% sort
#dim(train) # 43299 203

gc()



#######################
# Handle missing data #
#######################

for(i in 1:ncol(train)){
  cnt_na = sum(is.na(train[,i, with=FALSE]))
  if(cnt_na>0) print(c(names(train)[i], cnt_na, cnt_na/nrow(train)) )
}


#?knnImputation

system.time(
  #train_knnImp <- knnImputation(train, k=5, meth='median')
  #train_knnImp <- knnImputation(train, k=1)
  # Throws error:
  #   "Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric"
  # Same error for both methods (weighted average and median).
  # Same error when k=1
  train_centrImp <- centralImputation(train) # 1.06s elapsed
)

############################################
# Handle dangling problematic categoricals #
############################################

for(i in 1:ncol(train_centrImp)){
  feat = train_centrImp[,i, with=FALSE]
  if(is.factor(unlist(feat))|is.character(unlist(feat))) print(c(names(train_centrImp)[i], length(unique(unlist(feat)))))
  #print(c(names(train_centrImp)[i], class(unlist(feat))))
}

#head(train_centrImp[,.(gov, landareasquaremiles2010)])
#tail(train_centrImp[,.(landareasquaremiles2010)])

# Don't need to know who the governor is.
#train_centrImp[,gov:=NULL]


############
# Modeling #
############


#' Let's do a stack of models.
#' Model one: casualties?
#' Model two: deaths?

table(train$target)

#target0 = train[,target!="no_inj"]
#target1 = train[,target=="death"]
#mod_casualty = glm(as.factor(target=="no_inj")~., train, family=binomial)

system.time(
  mod_casualty_rf <- randomForest(x=train_centrImp[,-"target", with=FALSE], 
                                 y=train_centrImp[,as.factor(target=="no_inj")], 
                                 ntree=400,
                                 #mtry=10
  )
)
# 12.18  seconds for 10 trees (mtry=10). Yikes.
# 23.27  seconds for 20 trees (mtry=10). Linear growth
# 476.14 seconds for 400 trees (mtry=14(def))
## NB: this is overfitting on a single downsampling iteration.
importance(mod_casualty_rf)

# 99% accuracy on training data.
table(predict(mod_casualty_rf, train_centrImp), 
      train_centrImp$target=="no_inj") / nrow(train_centrImp)
table(predict(mod_casualty_rf, train), 
      train_centrImp$target=="no_inj") / nrow(train_centrImp)

save(mod_casualty_rf, file="models/mod_casualty_rf_prototype.Rdata")
##################################################################3

?glm
#mod_casualty = glm.fit(x=train[,-"target", with=FALSE], 
#                       y=train[,target=="no_inj"], 
#                       family=binomial(link='logit'),
#                       mustart=0 # why do I need to set this?
#                       )
                       #family=binomial())
# This model is throwing errors.
sum(is.na(train))

sum(is.na(train[,-"target", with=FALSE]))
sum(is.na(train[,target=="no_inj"]))
class(train[,target=="no_inj"])


# Let's try that again
system.time(
  mod_casualty_rf <- randomForest(x=train_centrImp[,-"target", with=FALSE], 
                                  y=as.factor(train_centrImp[,target=="no_inj"]), 
                                  ntree=10
  )
)
#  Can not handle categorical predictors with more than 53 categories.
#  oops.
system.time(
  mod_casualty <- glm(as.factor(target=="no_inj")~., train, family=binomial)
)
# 
# system.time(
#   mod_casualty_log <- glm.fit(x=train_centrImp[,-"target", with=FALSE],
#                               #x=as.data.frame(train_centrImp[,-"target", with=FALSE]), ## Doesn't make a difference...
#                               y=as.factor(train_centrImp[,target=="no_inj"]), 
#                               #family=binomial()
#                               family=binomial(link='logit')
#                               #,singular.ok = TRUE # Not a thing
#   )
# )

# Should probab