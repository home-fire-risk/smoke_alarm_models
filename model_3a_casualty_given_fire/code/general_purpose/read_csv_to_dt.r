source('code/general_purpose/standardize_variable_names.r')
library(data.table)
library(magrittr)

read_csv_to_dt = function(fpath){
  fpath %>%
    read.csv(stringsAsFactors=FALSE) %>%
    data.table %>%
    standardize_variable_names
}