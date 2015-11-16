
# Author: Andrew Brooks
# Date: 11-15-2015
# Purpose: Assign census block groups to each Red Cross HFP home visit.  We have the 
#          lat, long, but not the census tract/block code which we need for modeling.
#          This script is pretty hacky... couldn't find a free way to make a batch request
#          of reverse geocoding, so i parallelized and make a bunch of one-time calls.

library('foreach')
library('doMC')

# this the function that gets the block group for a pair of coordinates from the FCC API.
source('functions/coord_to_censusblock.R')

# setting directories
basedata_dir <- '/Users/ajb/Google Drive/Red Cross/smokealarm/data/RedCross/hfp_homevisits/' # google drive dir, or wherever the data lives
outdata_dir <- '/Users/ajb/Documents/redcross_hfp_geocoded' # where will the data be outputted.  Not dumping directly to Google Drive bc it messes up with the syncing

dt <- fread(sprintf('%s/HFC_Dupe_Check_Oct2014_Sept2015_Geocode.csv', basedata_dir), colClasses = c('Latitude'='character', 'Longitude'='character'))
dt$id <- 1:nrow(dt) # assign id to each visit. necessary for merging back to 

########################
# tests one at a time
########################

# toggling off for testing
if(1==0) {
  system.time(coord_to_censusblock(dt$Latitude, dt$Longitude, id=1:10, out=sprintf('%s/geocodes_1_10.csv', outdata_dir)))
  system.time(coord_to_censusblock(dt$Latitude, dt$Longitude, id=11:20, out=sprintf('%s/geocodes_11_20.csv', outdata_dir)))
  system.time(coord_to_censusblock(dt$Latitude, dt$Longitude, id=21:30, out=sprintf('%s/geocodes_21_30.csv', outdata_dir)))
}


########################
# Actually running in parallel with foreach
########################

registerDoMC(cores=10) # i have 4 cores on my macbook... but increasing beyond 4 seems to work
Sys.time()
ids <- split(1:nrow(dt), cut(1:nrow(dt), 50, labels=FALSE)) # real deal
#ids <- split(1:120, cut(1:120, 20, labels=FALSE)) # testing
system.time(
  foreach(i=ids) %dopar%
    coord_to_censusblock(dt$Latitude, dt$Longitude, id=i, out=sprintf('%s/geocodes_%s_%s.csv', outdata_dir, min(i), max(i)))
)
Sys.time()

########################
# Reassembling Geocoded Data
########################

# get list of .csvs geocoded above w foreach
filenames <- paste(outdata_dir, list.files(outdata_dir, pattern='.csv'), sep='/')

# Read and append geocoded results
dt_geo <- do.call("rbind", lapply(filenames, fread, header = F))
names(dt_geo) <- c('id', 'Latitude', 'Longitude', 'block_group')

# merge block groups onto the original data
dtm <- merge(dt, dt_geo[,.(id, block_group)], by='id', all.x=T, all.y=F)

# writing merged data out to csv
write.table(dtm, row.names=F, sep=',', sprintf('%s/HFC_Dupe_Check_Oct2014_Sept2015_Geocode_bg.csv', basedata_dir))


