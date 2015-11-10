countyChoroOverhead <- function(path="/Users/ajb/Google Drive/Red Cross/smokealarm/data/Census/gz_2010_us_050_00_5m") {
  # ``` this loads the necessary overhead to plot a series at the county level```
  library('ggmap')
  library('rgdal')
  US.counties <- readOGR(dsn=path, layer="gz_2010_us_050_00_5m")
  US.counties <- US.counties[!US.counties$STATE %in% c('02', '15', '72'),] # removing AK, HI, PR from plots
  county.data <- US.counties@data
  county.data$id <- rownames(county.data)
  county.data$county5_code <- paste0(county.data$STATE, county.data$COUNTY)
  
  us.dat <- fortify(US.counties)
  us.dat <- merge(us.dat, county.data, by='id')
  
  library('UScensus2010')
  library('stringr')
  data(countyfips)
  countyfips$state_num <- as.numeric(substr(countyfips$fips,0,2))
  stlookup <- countyfips[duplicated(countyfips$state_num)==F,]
  names(stlookup)[names(stlookup)=='acronym'] <- 'state_code'
  stlookup$state_num <- str_pad(stlookup$state_num, width=2, pad='0')
  
  names(us.dat)[names(us.dat)=='STATE'] <- 'state_num'
  us.dat$state_code <- stlookup$state_code[match(us.dat$state_num, stlookup$state_num)]  # bringing state_code to us.dat
  
  mstate <- map_data('state')
  
  retL <- list(us.dat=us.dat, mstate=mstate)
  return(retL)
}


countyChoro <- function(ser, brks, pal, df=cnagg2, mapdata=us.dat, mstate=mstate, setNA=NULL, main=''){
  if(is.null(setNA)==F) df[which(setNA), ser] <- NA
  serint <- as.integer(cut(df[,ser], breaks=brks, include.lowest=T))
  mapdata$int <- factor(serint[match(mapdata[,'county5_code'], as.character(df[,'county5_code']))])
  sercol <- cut(df[,ser], breaks=brks, include.lowest=T)
  labs <- paste(table(serint), levels(sercol)[which(table(sercol)>0)])
  gg <- ggplot(mapdata, aes(x=long, y=lat, group=group, fill=int)) +  geom_polygon(color='NA') +
    scale_fill_manual("Freq (low, high)", values=pal[which(table(sercol)>0)], labels=labs) +
    geom_polygon(data=mstate, fill=NA, color='black') +
    ggtitle(main)
}

st2num <- function(x) {
  # ```this function turns a vector of state abbreviations into their corresponding numeric codes```
  data(countyfips)
  countyfips$state_num <- as.numeric(substr(countyfips$fips,0,2))
  stlookup <- countyfips[duplicated(countyfips$state_num)==F,]
  names(stlookup)[names(stlookup)=='acronym'] <- 'state_code'
  stlookup$state_num <- str_pad(stlookup$state_num, width=2, pad='0')
  stlookup$state_code <- toupper(stlookup$state_code)
  ret <- stlookup$state_num[match(x, stlookup$state_code)]
  return(ret)
}