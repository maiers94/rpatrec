#library(np)

#'Perform Kernel Regression on Time Series Data
#'
#'@param input Vector of Time Series Data
#'@param bandwidth -Numerical: Choice of Bandwith
#'    -\code{auto}: Choose bandwith by Cross validation automatically for the given sample
#'
#'@return Vector containing smoothed time series data, prints the bandwidth used.
#'
#'
#'@importFrom stats fitted

kernel <- function(input, bandwidth="auto"){
  inputchecks(list(input,bandwidth),"kernel")

  x <- seq(1,length(input))

  if(is.numeric(bandwidth))reg <- np::npreg(input ~ x, ckertype = "gaussian", bws = bandwidth, regtype="lc")
  else if(bandwidth=="auto")reg <- np::npreg(input ~ x, ckertype = "gaussian", bwmethod="cv.ls", regtype="lc")
  else stop("invalid bandwidth input")

  output <- fitted(reg)
  print(reg[[1]])
  return(output)
}
