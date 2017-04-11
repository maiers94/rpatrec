#'Uses the inbuilt function \code{smooth.spline()} to smooth the data using splines.
#'
#'This function is purely included to provide the standard interface coherent with other smoothers to the user.
#'
#' @param input Time series data passed to \code{smooth.spline()}
#' @param ... Optional: Other arguments passed to \code{smooth.spline()}
#'
#' @return Smoothed time series data only, no additional output. Use \code{smooth.spline()} for greater functionality
#' @export

splines <- function(input,...){
  result <- smooth.spline(x=input,...)
  return(result[[2]])
}
