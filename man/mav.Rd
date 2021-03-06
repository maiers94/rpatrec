% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoother1.R
\name{mav}
\alias{mav}
\title{Compute the moving average, exponential average or running median.}
\usage{
mav(input, len = 10, method)
}
\arguments{
\item{input}{Time series data to use for computation}

\item{len}{An integer to determine the number of datapoints used for computation}

\item{method}{String. Determines the method of computation.
Permissible values are \code{exponential}, \code{simple} and \code{median}}
}
\value{
Vector containing the smoothed time series data of length of \code{input} less length of \code{len}
}
\description{
Use this function to compute any of these three moving average methods. These are the
simplest smoothers available in the package so it may be a good idea to start initial testing
with this function.
}
\details{
For an overview of the package capabilities, click here \link{rpatrec}.
}
\examples{
\dontrun{
#create a standard HS pattern:
a <- generator()
#add noise to this patterns
b <- noise(a,'white',10)
#smooth to regain the signal
c <- kernel(b,2)
}
##simply test the smoother
mav(1:10,5,'exponential')

}
