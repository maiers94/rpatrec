#'Perform Kernel Regression on Time Series Data
#'
#'Perform kernel regression in line with Lo et al. (2000). Either specify a bandwidth or let it be determined
#' automatically.
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'
#'@param input Vector of Time Series Data
#'@param bandwidth \describe{
#'\item{"Numerical"}{ Choice of Bandwith}
#'\item{\code{auto}}{ Choose bandwith by Cross validation automatically for the given sample}
#'}
#'
#'
#'@return Vector containing smoothed time series data, prints the bandwidth used.
#'
#'@export
#'@importFrom stats fitted
#'@import np
#'
#'@examples
#'\dontrun{
#'#create a standard HS pattern:
#'a <- generator()
#'#add noise to this patterns
#'b <- noise(a,"white",10)
#'#smooth to regain the signal
#'c <- kernel(b,2)
#'}
#'
#'

kernel <- function(input, bandwidth="auto"){
  inputchecks(list(input,bandwidth),"kernel")

  x <- seq(1,length(input))

  if(is.numeric(bandwidth))reg <- np::npreg(input ~ x, ckertype = "gaussian", bws = bandwidth, regtype="lc")
  else if(bandwidth=="auto")reg <- np::npreg(input ~ x, ckertype = "gaussian", bwmethod="cv.ls", regtype="lc")
  else stop("invalid bandwidth input")

  output <- fitted(reg)
  #print(reg[[1]])
  return(output)
}
