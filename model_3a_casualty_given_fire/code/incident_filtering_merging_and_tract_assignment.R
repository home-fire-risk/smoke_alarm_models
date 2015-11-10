library(data.table)

#' This contains the "data_all" object, which contains NFIRS database tables.
load("data/rdata/2015-09-16.RData")

#' Let's use zcta-tract relationships to assign tracts to zip codes. Where there is a many to one or many to many, use all
#' permutations.
zcta = read.csv("data/raw/census/zcta_tract_rel_10.txt"
                ,colClasses="character"
)
zcta   = data.table(zcta)
setnames(zcta, names(zcta), tolower(names(zcta)))
zcta_tract = zcta[,.(zcta5, state, county, tract, geoid)] 
setkey(zcta_tract, zcta5)


#' modularized process of NFIRS data merging and filtering. 
#' Repeat independently for all years of data to assign census tracts
#' and extract combine relevant incident data.
nfirst_filtered_tract__workhorse = function(
  inc_basic,
  inc_address
){
  # Assert data.table with lowercase field names
  inc_basic = data.table(inc_basic)
  setnames(inc_basic, names(inc_basic), tolower(names(inc_basic)))
  inc_address = data.table(inc_address)
  setnames(inc_address, names(inc_address), tolower(names(inc_address)))
  
  # Filter Columns
  inc_basic = inc_basic[,
                        .(fdid,
                          inc_date,
                          inc_no,
                          ff_death, 
                          oth_death, 
                          ff_inj, 
                          oth_inj, 
                          mixed_use, 
                          prop_use,
                          inc_type
                        )]
  
  # Filter rows
  inc_basic = inc_basic[
    mixed_use %in% c(
      '40' # residential
      ,'58' # business and residential
    ) | (
      mixed_use %in% c(
        'NN' # Not mixed use
        ,'00' # other
      ) &
        prop_use %in% c(
          '400' # Residential, other.
          ,'419' # 1 or 2 family dwelling.
          ,'429' # Multifamily dwelling.
          ,'439' # Boarding/rooming house, residential hotels.
          ,'449' # Hotel/motel, commercial.
          ,'459' # Residential board and care.
          ,'460' # Dormitory-type residence, other.
          ,'462' # Sorority house, fraternity house.
          ,'464' # Barracks, dormitory.
        )
    ) | (
      # Add incident types that are suggestive of relevant structure types
      inc_type %in% c(
        '120'  # - fire in mobile property used as a fixed structure, other
        ,'121' # - fire in mobile home used as a fixed residence
        ,'122' # - fire in motor home, camper, recreational vehicle
        ,'123' # - fire in portable building fixed location
        ,'136' # Self-propelled motor home or recreational vehicle.
        ,'137' # Camper or recreational vehicle (RV) fire.
      )
    )]
  
  # Associate tracts with incidents
  inc_zip = inc_address[,.(fdid, inc_date, inc_no, zip5)]
  setkey(inc_zip, zip5)
  inc_tract = zcta_tract[inc_zip, allow.cartesian=TRUE]
  
  # Merge tract assignments with incident attributes
  setkey(inc_basic, fdid, inc_date, inc_no)
  setkey(inc_tract, fdid, inc_date, inc_no) 
  inc_tract[inc_basic]
}

# Wrap workhorse function in parent function to ensure garbage collection 
# is accomplished after each call
nfirst_filtered_tract = function(inc_basic, inc_address){
  inc = nfirst_filtered_tract__workhorse(inc_basic, inc_address)
  gc()
  inc
}

#names(data_all) # we've really only got two years of data here :(

# 4.43s 
system.time(
  inc2010 <- nfirst_filtered_tract(data_all[['2010basicincident']],
                                   data_all[['2010incidentaddress']])
)

# 10.87 s
system.time(
  inc2011 <- nfirst_filtered_tract(data_all[['2011basicincident']],
                                   data_all[['2011incidentaddress']])
)

inc = rbindlist(list(inc2010, inc2011))
rm(inc2010)
rm(inc2011)
gc()


# Let's save this object
save(inc, file="data/rdata/inc.rdata")

if(FALSE){
  dim(inc) # 4139054      12
  inc
  table(inc[,oth_inj + oth_death>0]) / nrow(inc) 
  # Only 1.6% of incidents resulted in non-FF injury or death. 
  table(inc[,oth_death>0]) / nrow(inc) 
  # Only 0.26% of injuries resulted in death. 
  
  # I mean... that's great, but that's a pretty rough class imbalance.
  # I shold probably roll in RC responses as well. Need to think of a good
  # way to associate these with fire incidents to flag which incidents resulted
  # in a RC response.
}