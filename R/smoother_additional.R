#'Use the inbuilt function \link{smooth.spline} to smooth time series data
#'
#'This function provides smoothing capabilities using the cubic B-spline
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'This function is purely included to provide the standard interface coherent with other smoothers to the user.
#'
#' @param input Time series data passed to be smoothed
#' @param spar Smoothing Parameter, value should be between 0 and 1. \code{NULL} for automatic computation
#' @param ... Optional: Other arguments passed to \link{smooth.spline}
#'
#' @return Smoothed time series data only, no additional output.
#' @importFrom stats smooth.spline
#' @export
#' @examples
#' \dontrun{
#'#create a standard HS pattern:
#'a <- generator()
#'#add noise to this patterns
#'b <- noise(a,'white',10)
#'#smooth to regain the signal
#'c <- splines(b)
#'}
#'

splines <- function(input, spar = 0.5, ...) {
  result <- smooth.spline(x = input, spar = spar, ...)
  return(result[[2]])
}

#'Use the inbuilt function \link{loess} to smooth time series data.
#'
#'Use local regression to fit a global non-parametric model to the data. If the span is
#'smaller than 1 the regression is truly local, if it is larger than 1 all data points in the sample are taken into account
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'This function is purely included to provide the standard interface coherent with other smoothers to the user.
#'
#' @param input Time series data passed to be smoothed
#' @param span The main smoothing parameter.
#' @param ... Optional: Other arguments passed to \link{loess}
#'
#' @return Smoothed time series data only, no additional output.
#' @importFrom stats loess
#' @export
#' @examples
#' \dontrun{
#'#create a standard HS pattern:
#'a <- generator()
#'#add noise to this patterns
#'b <- noise(a,'white',10)
#'#smooth to regain the signal
#'c <- loess.rpatrec(b)
#'}

loess.rpatrec <- function(input, span = 0.75, ...) {
  s <- seq(1, length(input))
  result <- loess(input ~ s, span = span, ...)
  return(result[[2]])
}
