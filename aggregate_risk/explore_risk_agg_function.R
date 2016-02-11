#' ---
#' title: "Testing Risk Aggregation Function"
#' author: Andrew Brooks
#' date: February 11, 2016
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

library('rmarkdown')
library('knitr')
library('Rsenal') # devtools::install_github('brooksandrew/Rsenal')

#' ## Defining some data to test on
x1 <- abs(rnorm(1000, 3, 1))
x2 <- log(abs(rnorm(1000, 3, 1)))
x3 <- abs(rnorm(1000, 3, 1))^3
x4 <- c(abs(rnorm(995, 3, 1))^3, rnorm(5, 10, .05)^3)

#' ## Define aggregation function
normalize <- function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T)- min(x, na.rm=T))
balscale <- function(x, w_logit=0.85, w_normalized=0.15) plogis(scale(x))*w_logit + normalize(x)*w_normalized

#' `balscale` is the ultimate scaling function used to get all risk indicators from the various models on the same scale ranging from 0 to 100.
#' It is comprised of two parts.
#' 
#' **1) Logistic function of Z-Scores:**  Taking Z-scores of each metric would get all indicators on the same scale with similar distributions,
#' (same means quantiles, etc.).  However, the scale is centered at 0 (includes negatives) and theoretically unbounded.  We desire a bounded scale between 0 and 1.
#' So we pass these Z scores through a logistic function which essentially squashes the data.  Rank order is preserved, but the distance between 
#' values does change (illustrated in the charts below).  Note also that to maintain the same distribution across variables (each variable averages to 0.5)
#' some scaled variables using this method won't start until a number distinguishably greater than 0, (~0.3 for instance in one of the examples).  Likewise, some variables
#' might max out at a number between 0.5 and 1 instead of 1.  
#'   
#'   
#' **2) Feature Scaling (x - xmin)/(xmax-xmin):** Traditional feature scaling method.  Keeps the exact same distribution of raw data, just reassigns 
#' scale to be between 0 and 1.  This is sensitive to outliers and theoretically could return a value > 1 when scoring new data unless values for the old
#' data are changed.  
#'   
#' I experimented with both of these.  Curently using a weighted measure which is 85% logistic function of Z scores and 15% feature scaling.
#' The 15% feature scaling allows the data to speak more (preserving more of the original distance between values in the tails) while keeping outliers in check
#' with it's low weight of 15%.
#' 
#' ## Defining function to check the distribution of each 

compareFunction <- function(x) {
  xname <- deparse(substitute(x))
  xs <- balscale(x)
  hist(x, main=paste0("raw data: ", xname), breaks=100)
  hist(xs, main=paste0("scaled data: ", xname), breaks=100, xlim=c(0,1))
  #plot(sort(x), main = paste0("raw data sorted: ", xname), xlab="index")
  #plot(sort(xs), main = paste0("scaled data sorted: ", xname), xlab="index")
  plot(x, xs, main= paste0("scaled vs raw data: ", xname), xlab="raw data", ylab="scaled data", cex=0.6)
  kable(do.call(cbind, apply(data.frame(x=x, norm_x=normalize(x), xs=xs), 2, function(x) Rsenal::quantile_df(x, seq(0, 1, .1))))[-c(3,5)])
}

#' ## Inspecting scaling function on examples

#' ### Example 1:  Normal Distribution
#+ fig.width=4.5, fig.height=4.5
compareFunction(x1)

#' ### Example 2:  Left Skew
#+ fig.width=4.5, fig.height=4.5
compareFunction(x2)

#' ### Example 3: Right Skew
#+ fig.width=4.5, fig.height=4.5
compareFunction(x3)

#' ### Example 4:  Right Skew with Big Outliers
#+ fig.width=4.5, fig.height=4.5
compareFunction(x4)

# rmarkdown::render('aggregate_risk/explore_risk_agg_function.R', output_file = 'explore_risk_agg_function.html')

