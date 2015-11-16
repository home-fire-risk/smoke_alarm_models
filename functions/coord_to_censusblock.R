
# This function assigns a census block group to a pair of lat/lon coordinates.
# Uses this free API from the FCC https://www.fcc.gov/developers/census-block-conversions-api

library('XML')
library('data.table')

coord_to_censusblock <- function(lat, long, id=NULL, out='geocodes.csv', overwrite=T, progress=T){
  if(is.null(id)) id <- 1:length(lat)
  if(length(lat)!=length(long)) stop('lat and long no of equal length')
  if(overwrite==T & out %in% list.files()) {file.remove(out); cat(sprintf('removing %s \n', out))}
  pbmax <- min(20,length(id))
  pb <- txtProgressBar(min=min(id), max=max(id), initial=0, char="=", style=3)
  
  for(i in id){
    url <- sprintf('http://data.fcc.gov/api/block/2010/find?latitude=%s&longitude=%s', lat[i], long[i])
    doc <-  xmlTreeParse(url)
    tryget <- try({block <- as.character(doc[[1]]$children$Response['Block']$Block)[2]}) # messy way to scrape off census block
    if(class(tryget)=='try-error') block <- 'error'
    ret <- data.frame(i, lat[i], long[i], block)
    write.table(ret, file = out, sep = ",",  col.names=FALSE, row.names=FALSE, append=TRUE)
    # if(i %in% seq(min(id), max(id), by=round((abs(max(id)-min(id))/pbmax)))) setTxtProgressBar(pb, i)
    setTxtProgressBar(pb, i)
  }
  return(ret)
}


