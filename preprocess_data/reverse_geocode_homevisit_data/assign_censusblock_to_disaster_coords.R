
# Author: Margaret Furr
# Date: 1/31/2016
# Purpose: Getting tracts for coordinate pairs to prepare for calculating a risk indicator

# libraries
library('XML')
library('httr')
library('base')
library('rjson')

# read data from 2009-2014_RedCross_DisasterCases_sample.csv
redcross_disaster_cases <- read.csv("2009-2014_RedCross_DisasterCases_sample.csv",header=TRUE,na=NA)
#redcross_disaster_cases <- read.csv("/Users/abrooks/Google Drive/Red Cross/smokealarm/data/RedCross/redcross_disaster_cases/2009-2014_RedCross_DisasterCases_sample.csv",header=TRUE)

# home fire cases
redcross_homefire_cases <- redcross_disaster_cases[redcross_disaster_cases$event_type_old_categories == "Fire : Multi-Family" | redcross_disaster_cases$event_type_old_categories == "Fire : Single Family" | redcross_disaster_cases$event_type_old_categories == "Fire",]

# lat/lon coordinates = esri_latitude_x and esri_longitude_x 
redcross_homefire_cases$esri_latitude_x[1:5]
redcross_homefire_cases$esri_longitude_x[1:5]

# census tract
redcross_homefire_cases$census_tract <- c()
# urls
urls <- c()

# example of reading url 
system.time({
  for (i in (1:nrow(redcross_homefire_cases))) {
    if(is.na(redcross_homefire_cases$esri_latitude_x[i])!=TRUE) {
      urls[i] <- paste0("http://www.broadbandmap.gov/broadbandmap/census/tract?latitude=", 
                        redcross_homefire_cases$esri_latitude_x[i],
                        "&longitude=",
                        redcross_homefire_cases$esri_longitude_x[i],
                        "&format=xml"
                        )
      
      doc <-  xmlTreeParse(urls[i])
      tract <- xmlSApply(xmlRoot(doc)[[1]][[1]], xmlValue)['fips']
      redcross_homefire_cases$tract[i] <- tract
      if(i %% 10 == 0) cat(paste0(i, " records completely matched \n"))
    }
  }
})

# redcross_homefire_cases
redcross_homefire_cases <- data.frame(lapply(redcross_homefire_cases, as.character), stringsAsFactors=FALSE)
redcross_homefire_cases <- redcross_homefire_cases[,c(1:39,43)]
write.csv(redcross_homefire_cases, "2009-2014_RedCross_Homefire_Cases.csv")

