#' ---
#' title: Explore AHS raw data
#' author: Andrew Brooks
#' date: September 21, 2015
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#' #### Getting setup
library('data.table') # need version 1.9.7 from github (Rdatatable/datatable)
library('knitr')
library('maps') # used for state fips code
library('stringr')

repodir <- '/Users/ajb/Documents/github/arc_smoke_alarm/'
setwd(repodir)

# opts_knit$set(root.dir = repodir)
opts_knit$set(warning = F)

#' <!-- ####################################### -->
#' <!-- ####################################### -->
#' ## Gather and Load AHS Data
#' <!-- ####################################### -->
#' <!-- ####################################### -->

#' This data is downloaded directly from the [Census AHS website here](http://www.census.gov/programs-surveys/ahs/data/2011/ahs-national-and-metropolitan-puf-microdata.html). 
#' I grabbed [AHS 2011 National and Metropolitan PUF v1.4 CSV](http://www2.census.gov/programs-surveys/ahs/2011/AHS%202011%20National%20and%20Metropolitan%20PUF%20v1.4%20CSV.zip)
#' Note 2013 AHS survey data is available, but does not contain questions related to smoke alarm installs.  

#' **Codebook:** This [codebook](http://www.census.gov/content/dam/Census/programs-surveys/ahs/tech-documentation/AHS%20Codebook%202013.pdf) is an essential resource.  

#' the **tnewhouse** is the primary table we are interested in.  There are supplementary tables which provide information about other topics.

# data too big to store in repo.  figuring out best place to store it.  not used for anything beyond exploration for now.
system.time(nh <- fread('data/Census/AHS_2011_PUF/tnewhouse.csv', sep=',', stringsAsFactors=F))
#system.time(wt <- fread('data/Census/AHS_2011_PUF/tRepwgt.csv', sep=',', stringsAsFactors=F)

#' <!-- ####################################### -->
#' <!-- ####################################### -->
#' ## Cleaning Up
#' <!-- ####################################### -->
#' <!-- ####################################### -->

#' function to remove annoying ticks in raw data.  `fread` does not currently support the `quote` argument of `read.csv` :(.
removeFirstLastChar <- function(x, first=1, last=1) {
  substr(x, 2, nchar(x)-1)
}

#' Actually removing annoying ticks in data for variables of interest
nh_vars_char <- c('CONTROL', 'COUNTY', 'STATE', 'DIVISION', 'SMOKE', 'SMOKPWR', 'SPRNKLR', 'BATTERY')
for(i in nh_vars_char) set(nh, j=which(i==names(nh)), value=gsub("'", "", nh[[i]]))


#' Adding state abbreviation
data(state.fips)
sf <- data.table(state.fips)
sf[,STATE:=as.character(fips)]
sf[,STATE:=str_pad(STATE, width=2, side='left', pad='0')]  # for merge

#' Adding state abbrevation with match
nh[,abb:=sf$abb[match(STATE, sf$STATE)]]

#' There are some counties that occur in many states.  Must aggregate by `STATE` & `COUNTY`
print(nh[,.(list(unique(abb))), by='COUNTY'])

# Removing COUNTIES with less than 20 records
nh <- nh[,tmp:=.N ,by=c('COUNTY', 'STATE')][tmp>30,]


#' <!-- ####################################### -->
#' <!-- ####################################### -->
#' ## Exploratory Data Analysis
#' <!-- ####################################### -->
#' <!-- ####################################### -->

#' We only have AHS data for some states...
barplot(sort(table(nh$abb), decreasing=T), las=2, cex.names=.7, col='navy')

#' Prevelance of `SMOKE` by County  

#' **1** = Yes  
#' **2** = No  
#' **B** = or -6 Not applicable  
#' **D** = or -7 Don’t know  
#' **R** = or -8 Refused  
#' **Blank or -9** = Not reported  

#' Most households report a working smoke alarm
hist(nh[,sum(SMOKE=='1')/.N, by=c('COUNTY', 'STATE')]$V1, breaks=50, main='percent of households with working smoke alarm', xlab='')


# rmarkdown::render('code/ahs_smokealarm_ind/explore_ahs.R')
