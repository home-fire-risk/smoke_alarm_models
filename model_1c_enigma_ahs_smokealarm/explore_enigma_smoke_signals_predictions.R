#' ---
#' title: Explore Enigma's Smoke Signals Results at Block level
#' author: Andrew Brooks
#' date: October 4, 2015
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---



# loading dependencies
library('stringr')
library('data.table')
library('rmarkdown')

#' Assumes you have run Brian's code to generate smoke signal model results: https://github.com/enigma-io/smoke-signals-model 

repodir <- '/Users/ajb/Documents/github/arc_smoke_alarm/'
setwd(repodir) # commenting out when knitting 

ss <- fread('models/model_1c_enigma_ahs_smokealarm/results/smoke-alarm-risk-scores.csv')

#' ### separating out geographies. block from census tract
ss[,bg_geoid:=as.character(bg_geoid)]
ss[,block:=substr(bg_geoid, nchar(bg_geoid)-3, nchar(bg_geoid))]
ss[,census_tract:=gsub('.{4}$', '', bg_geoid)]
ss[,state:=gsub('.{6}$', '', census_tract)]
ss[,tract:=substr(census_tract, nchar(census_tract)-2L, nchar(census_tract))]
ss[,county:={tmp=gsub('.{3}$', '', census_tract); county=substr(tmp, nchar(tmp)-2, nchar(tmp))}]

#' ### Exploring coverage by state
table(ss[,state])

#' ### Calculating blocks per census tract
ss[,blocks_per_tract:=.N, by=census_tract]
hist(ss[,blocks_per_tract], col='navy', main='Blocks per census tract')

#' ### Aggregating to census tract level
ssc <- ss[,.(
  smoke_alarm_risk=mean(smoke_alarm_risk),
  ct_pop=sum(bg_pop),
  blocks_per_county=.N
), by=.(county, state)]
ssc[,county5_code:=paste0(str_pad(state, width=2, side='left', '0'), county)]

#' ### Mapping

# necessary overhead to make county map
source('functions/countyChoro.R')
mapoverhead <- countyChoroOverhead()

#' #### Quantile binning
#+ fig.width=12, fig.height=8
brks <- quantile(ssc[,smoke_alarm_risk], seq(0,1,length.out=10))
pal <- colorRampPalette(c('gray', '#5A0000'))(length(brks)-1)
df <- data.frame(ssc)
countyChoro(ser='smoke_alarm_risk', brks=brks, pal=pal, df=df, mstate=mapoverhead$mstate, mapdata=mapoverhead$us.dat) + 
  theme_bw() + ggtitle('Smoke Alarm Risk: Quantile binning')

#' #### Fixed width binning
#+ fig.width=12, fig.height=8
brks <- seq(min(ssc[,smoke_alarm_risk]), max(ssc[,smoke_alarm_risk]), length.out=10)
pal <- colorRampPalette(c('gray', '#5A0000'))(length(brks)-1)
df <- data.frame(ssc)
countyChoro(ser='smoke_alarm_risk', brks=brks, pal=pal, df=df, mstate=mapoverhead$mstate, mapdata=mapoverhead$us.dat) + 
  theme_bw() + ggtitle('Smoke Alarm Risk: Fixed width binning')

#' #### Number of blocks per county
#+ fig.width=12, fig.height=8
brks <- quantile(ssc[,blocks_per_county], seq(0,1,length.out=10))
pal <- colorRampPalette(c('gray', '#5A0000'))(length(brks)-1)
df <- data.frame(ssc)
countyChoro(ser='blocks_per_county', brks=brks, pal=pal, df=df, mstate=mapoverhead$mstate, mapdata=mapoverhead$us.dat) + 
  theme_bw() + ggtitle('Blocks per county')

# rmarkdown::render('models/model_1c_enigma_ahs_smokealarm/explore_enigma_smoke_signals_predictions.R')