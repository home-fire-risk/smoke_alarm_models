
# Author: Margaret Furr
# Date: 2/6/2016
# Purpose: Getting tracts for coordinate pairs to prepare for calculating a risk indicator

# libraries
library('readr')
library('plyr')
library('dplyr')
library('ggplot2')
library('stringr')
library('gdata')

# read data from 2009-2014_RedCross_DisasterCases_sample.csv
redcross_homefire_cases <- read.csv("2009-2014_RedCross_HomeFire_Cases.csv",header=TRUE,na=NA)

# tract
redcross_homefire_cases$tract

# order data
redcross_homefire_cases_orderedtracts <- redcross_homefire_cases[order(redcross_homefire_cases$tract),]
redcross_homefire_cases_orderedtracts_dates <- redcross_homefire_cases_orderedtracts[order(redcross_homefire_cases_orderedtracts$incident_disaster_date),]
redcross_homefire_cases[1:20,]

# aggregate data - number of cases per tract and day
aggregated_homefire_data1 <- aggregate(redcross_homefire_cases$num_case, by=list(redcross_homefire_cases$tract, redcross_homefire_cases$incident_disaster_date),FUN=sum)
# the above data is the same as this below, but different order:
#aggregated_homefire_data2 <- aggregate(redcross_homefire_cases$num_case, by=list(redcross_homefire_cases$incident_disaster_date, redcross_homefire_cases$tract),FUN=sum)
#aggregated_homefire_data1[order(aggregated_homefire_data1[,2]),][1:25,]
#aggregated_homefire_data2[order(aggregated_homefire_data2[1]),][1:25,]
aggregated_homefire_data <- aggregated_homefire_data1

# aggregate data - number of cases per tract
aggregated_homefire_data3 <- aggregate(redcross_homefire_cases$num_case, by=list(redcross_homefire_cases$tract),FUN=sum)
aggregated_homefire_data3$x

# risk indicator 1 - number of cases per tract and day
homefire_risk_indicator1 <- aggregated_homefire_data$x

# where there are no gaps in cases
which((aggregated_homefire_data[,3]=="NA")==FALSE)
nrow(aggregated_homefire_data) # 6832
nrow(data.frame(which((aggregated_homefire_data[,3]=="NA")==FALSE))) # 6811
num_na <- nrow(aggregated_homefire_data) - nrow(data.frame(which((aggregated_homefire_data[,3]=="NA")==FALSE)))
num_na # 21 NA

# risk indicator 2 - number of cases per tract
homefire_risk_indicator2 <- aggregated_homefire_data3$x

# rename risk indicator variable names
aggregated_homefire_data <- rename.vars(aggregated_homefire_data,"x","risk_indicator")
names(aggregated_homefire_data)
aggregated_homefire_data3 <- rename.vars(aggregated_homefire_data3,"x","risk_indicator")
names(aggregated_homefire_data3)

# write to csv
write.csv(aggregated_homefire_data, "homefire_risk_indicator1.csv")
write.csv(aggregated_homefire_data3, "homefire_risk_indicator3.csv")
