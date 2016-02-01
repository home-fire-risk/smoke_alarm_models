
# Author: Margaret Furr
# Date: 1/31/2016
# Purpose: Getting tracts for coordinate pairs to prepare for calculating a risk indicator
# Note: some code being used is coming from assign_censusblock_to_homevisit_coords.R

# foreach and doMC libraries
library('foreach')
library('doMC')

# read data from 2009-2014_RedCross_DisasterCases_sample.csv
redcross_disaster_cases <- read.csv("2009-2014_RedCross_DisasterCases_sample.csv",header=TRUE)

# out data dir
outdata_dir = 'Users/margaret/Desktop'

# lat/lon coordinates = esri_latitude_x and esri_longitude_x 
redcross_disaster_cases$esri_latitude_x[1:5]
redcross_disaster_cases$esri_longitude_x[1:5]

# home fire cases
redcross_homefire_cases <- redcross_disaster_cases[redcross_disaster_cases$event_type_new_categories == "Fire",]
redcross_homefire_cases$event_type_old_categories

# this the function that gets the block group for a pair of coordinates from the FCC API.
source('coord_to_censusblock.R')

# Note: code being used is coming from assign_censusblock_to_homevisit_coords.R
# parallel process with foreach to get block groups for pairs of coordinates from FCC API
ids <- split(1:nrow(redcross_disaster_cases), cut(1:nrow(redcross_disaster_cases), 50, labels=FALSE))
registerDoMC(cores=10) # i have 4 cores on my macbook... but increasing beyond 4 seems to work
Sys.time()
ids <- split(1:nrow(redcross_disaster_cases), cut(1:nrow(redcross_disaster_cases), 50, labels=FALSE)) # real deal
#ids <- split(1:120, cut(1:120, 20, labels=FALSE)) # testing
system.time(
  foreach(i=ids) %dopar%
    coord_to_censusblock(redcross_disaster_cases$esri_latitude_x, redcross_disaster_cases$esri_longitude_x, id=i, out=sprintf('%s/geocodes_%s_%s.csv', outdata_dir, min(i), max(i)))
)
Sys.time()

# get list of .csvs geocoded above w foreach
filenames <- paste(outdata_dir, list.files(outdata_dir, pattern='.csv'), sep='/')

# Read and append geocoded results
redcross_disaster_cases_geo <- do.call("rbind", lapply(filenames, fread, header = F))
names(redcross_disaster_cases_geo) <- c('id', 'Latitude', 'Longitude', 'block_group')

# merge block groups onto the original data
redcross_disaster_cases_m <- merge(redcross_disaster_cases, redcross_disaster_cases_geo[,.(id, block_group)], by='id', all.x=T, all.y=F)

# writing merged data out to csv
write.table(redcross_disaster_cases_m, row.names=F, sep=',', sprintf('2009-2014_RedCross_DisasterCases_sample_Geocode_bg.csv', 'Users/margaret/Desktop'))

