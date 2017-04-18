#'Uses the inbuilt function \code{smooth.spline()} to smooth the data using splines.
#'
#'This function is purely included to provide the standard interface coherent with other smoothers to the user.
#'
#' @param input Time series data passed to \code{smooth.spline()}
#' @param spar Smoothing Parameter, value should be between 0 and 1. \code{NULL} for automatic computation
#' @param ... Optional: Other arguments passed to \code{smooth.spline()}
#'
#' @return Smoothed time series data only, no additional output. Use \code{smooth.spline()} for greater functionality
#' @importFrom stats smooth.spline
#' @export

splines <- function(input,spar=NULL,...){
  result <- smooth.spline(x=input,spar=spar,...)
  return(result[[2]])
}

#'Uses the inbuilt function \code{loess()} to smooth the data using local polynomial regression.
#'
#'This function is purely included to provide the standard interface coherent with other smoothers to the user.
#'
#' @param input Time series data passed to \code{loess()}
#' @param span The main smoothing parameter.
#' @param ... Optional: Other arguments passed to \code{loess()}
#'
#' @return Smoothed time series data only, no additional output. Use \code{smooth.spline()} for greater functionality
#' @importFrom stats loess
#' @export

loess.rpatrec <- function(input,span=0.75,...){
  s <- seq(1,length(input))
  result <- loess(input~s,span=span,...)
  return(result[[2]])
}
