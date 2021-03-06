% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoother_additional.R
\name{splines}
\alias{splines}
\title{Use the inbuilt function \link{smooth.spline} to smooth time series data}
\usage{
splines(input, spar = 0.5, ...)
}
\arguments{
\item{input}{Time series data passed to be smoothed}

\item{spar}{Smoothing Parameter, value should be between 0 and 1. \code{NULL} for automatic computation}

\item{...}{Optional: Other arguments passed to \link{smooth.spline}}
}
\value{
Smoothed time series data only, no additional output.
}
\description{
This function provides smoothing capabilities using the cubic B-spline
}
\details{
For an overview of the package capabilities, click here \link{rpatrec}.
This function is purely included to provide the standard interface coherent with other smoothers to the user.
}
\examples{
\dontrun{
#create a standard HS pattern:
a <- generator()
#add noise to this patterns
b <- noise(a,'white',10)
#smooth to regain the signal
c <- splines(b)
}

}
