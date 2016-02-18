

# Author: Margaret Furr
# Date: 1/31/2016
# Purpose: Getting tracts for coordinate pairs to prepare for calculating a risk indicator

# libraries
library('XML')
library('httr')
library('base')
library('rjson')
library('base')
library('foreach')
library('doMC')
library('data.table')
library('plyr')

outdata_dir <- '/Users/margaret/Desktop/Coordinates' # where will the data be outputted.  Not dumping directly to Google Drive bc it messes up with the syncing

# read data from 2009-2014_RedCross_DisasterCases_sample.csv
redcross_disaster_cases <- read.csv("2009-2014_RedCross_DisasterCases.csv",header=TRUE,na=NA)
nrow(redcross_disaster_cases)

# home fire cases
redcross_homefire_cases <- redcross_disaster_cases[redcross_disaster_cases$event_type_old_categories == "Fire : Multi-Family" | redcross_disaster_cases$event_type_old_categories == "Fire : Single Family" | redcross_disaster_cases$event_type_old_categories == "Fire",]

# lat/lon coordinates = esri_latitude_x and esri_longitude_x 
redcross_homefire_cases$esri_latitude_x[1:5]
redcross_homefire_cases$esri_longitude_x[1:5]

# census tract
redcross_homefire_cases$census_tract <- c()
# urls
urls <- c()

coord_to_censusblock <- function(lat, long, id=NULL, out='geocodes.csv', overwrite=T, progress=T){
  pbmax <- min(20,length(id))
  pb <- txtProgressBar(min=min(id), max=max(id), initial=0, char="=", style=3)
  
  for(i in id){
    if(is.na(redcross_homefire_cases$esri_latitude_x[i])!=TRUE&&is.na(redcross_homefire_cases$esri_longitude_x[i])!=TRUE) {
      url <- paste0("http://www.broadbandmap.gov/broadbandmap/census/tract?latitude=", 
                    redcross_homefire_cases$esri_latitude_x[i],
                    "&longitude=",
                    redcross_homefire_cases$esri_longitude_x[i],
                    "&format=xml"
                    )
      doc <-  xmlTreeParse(url)
      tract <- xmlSApply(xmlRoot(doc)[[1]][[1]], xmlValue)['fips']
      #tryget <- try({block <- as.character(doc[[1]]$children$Response['Block']$Block)[2]}) # messy way to scrape off census block
      #if(class(tryget)=='try-error') block <- 'error'
      ret <- data.frame(i, lat[i], long[i], tract)
      if(i %% 10 == 0) cat(paste0(i, " records completely matched \n"))
      write.table(ret, file = out, sep = ",",  col.names=FALSE, row.names=FALSE, append=TRUE)
      # if(i %in% seq(min(id), max(id), by=round((abs(max(id)-min(id))/pbmax)))) setTxtProgressBar(pb, i)
      setTxtProgressBar(pb, i)
    }
  }
}

registerDoMC(cores=10) # i have 4 cores on my macbook... but increasing beyond 4 seems to work
ids <- split(1:nrow(redcross_homefire_cases), cut(1:nrow(redcross_homefire_cases), 7000, labels=FALSE)) # real deal
#ids <- split(1:120, cut(1:120, 20, labels=FALSE)) # testing
system.time(
  foreach(i=ids) %dopar%
    coord_to_censusblock(redcross_homefire_cases$esri_latitude_x, redcross_homefire_cases$esri_longitude_x, id=i, out=sprintf('%s/geocodes_%s_%s.csv', outdata_dir, min(i), max(i)))
)


# get list of .csvs geocoded above w foreach
filenames <- paste(outdata_dir, list.files(outdata_dir, pattern='.csv'), sep='/')

# Read and append geocoded results
homefire_geo <- do.call("rbind", lapply(filenames, fread, header = F))
names(homefire_geo) <- c('id', 'Latitude', 'Longitude', 'tract')

write.csv(homefire_geo, "2009-2014_Homefire_geo.csv")

homefire_geo <- read.csv("2009-2014_Homefire_geo.csv")
nrow(redcross_homefire_cases) - nrow(homefire_geo) # 59336 missing
fires_per_tract <- count(homefire_geo$tract)
write.csv(fires_per_tract, "fires_per_tract.csv")
