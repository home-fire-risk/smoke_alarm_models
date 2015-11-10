
unbalanced_downsample = function(target, class_names, perc, boot=TRUE, n=NA){
  #' Downsamples a dataset to achieve a given class balance. 
  #' Assumes the first-indexed class is the "primary" class,
  #' on which the sample sizes for the other classes will be 
  #' determined.
  #' 
  #' target: a vector of class assignments
  #' class_names: a vector giving the unique class names in 
  #'   order corresponding to the perc vector. The first
  #'   class name is assumed to be the "primary" class
  #' perc: the desired class distribution percentages for the
  #'   output.
  #' boot: whether or not to sample with replacement.
  
  # input data assertions
  stopifnot(sum(perc)==1)
  stopifnot(length(class_names)==length(perc))
  stopifnot(length(class_names)==length(unique(target)))
  
  primary = which(target==class_names[1])
  
  if(boot){
    if(is.na(n)) n=length(primary)
    sampleMethod = function(x, n) sample(x, n, replace=TRUE)
  } else {
    sampleMethod = function(x, n) sample(x, n, replace=FALSE)
  }
  
  tot = ceiling(n/perc[1]) # maybe make tot an input param as well
  
  ix = list()
  ix[[class_names[1]]] = sampleMethod(primary, n)
  
  for(j in 2:length(class_names)){
    this_class = class_names[j]
    class_ix = which(target==this_class)
    n_j = floor(perc[j]*tot)
    ix[[this_class]] = sampleMethod(class_ix, n_j)
  }
  ix
}

if(FALSE){
  test = c(rep("a", 15), rep("b", 15), rep("c", 30))
  unbalanced_downsample(test, c("a","b","c"), c(.50,.25,.25), boot=TRUE, n=NA)
  # awesome
}
