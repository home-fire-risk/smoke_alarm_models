library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(stargazer)

setwd("/users/nickbecker/documents/github/red_cross_smoke_alarm_testing/")

rc_disaster_data = read_csv("/users/nickbecker/downloads/2009-2014_RedCross_DisasterCases.csv")
str(rc_disaster_data)
glimpse(rc_disaster_data)

#load("/Users/nickbecker/Documents/Github/Superseded/arc_smoke_alarm_old_2/data_from_david/2015-09-16.Rdata") 
#all.equal(rc_disaster_data$case_num, RC_response$case_num)

rc_disaster_data = as.data.frame(rc_disaster_data)

# Basic summary statistics
stargazer(rc_disaster_data, type = "text", median = TRUE, iqr = TRUE,
          out = "rc_disaster_summary_stats.txt")

summary(rc_disaster_data)













































