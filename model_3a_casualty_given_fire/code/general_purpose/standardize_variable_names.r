standardize_variable_names = function(obj){
  oldnames = names(obj)
  newnames <- oldnames %>% tolower %>% gsub("[.]+","_", .) 
  if("data.table" %in% class(obj)){
    require(data.table)
    setnames(obj, oldnames, newnames)
  } else {
    names(obj) = newnames
  }
  obj
}