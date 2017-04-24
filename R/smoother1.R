#'Compute the moving average, exponential average or running median.
#'
#'Use this function to compute any of these three moving average methods. These are the
#'simplest smoothers available in the package so it may be a good idea to start initial testing
#'with this function.
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'
#'@param input Time series data to use for computation
#'@param len An integer to determine the number of datapoints used for computation
#'@param method String. Determines the method of computation.
#'    Permissible values are \code{exponential}, \code{simple} and \code{median}
#'
#'@return Vector containing the smoothed time series data of length of \code{input} less length of \code{len}
#'
#'
#'@examples
#'\dontrun{
#'#create a standard HS pattern:
#'a <- generator()
#'#add noise to this patterns
#'b <- noise(a,'white',10)
#'#smooth to regain the signal
#'c <- kernel(b,2)
#'}
#'##simply test the smoother
#'mav(1:10,5,'exponential')
#'
#'@export
#'@importFrom stats median
#'


mav <- function(input, len = 10, method) {

  inputchecks(list(input, len, method), "mav")

  #########
  sim <- function(inp, l) {
    output <- vector(length = (length(inp) - l))
    for (i in 1:(length(inp) - l)) {
      output[i] <- sum(inp[i:(i + len) - 1])/l
    }
    return(output)
  }
  ##########
  med <- function(inp, l) {
    output <- vector(length = (length(inp) - l))
    for (i in 1:(length(inp) - l)) {
      output[i] <- median(inp[i:(i + len)])
    }
    return(output)
  }
  ##########
  exp <- function(inp, l) {
    output <- vector(length = (length(inp) - l))
    output[1] <- sum(inp[1:l])/l
    alpha <- 2/(l + 1)
    for (i in 2:(length(inp) - l)) {
      output[i] <- (inp[i + l] - output[i - 1]) * alpha + output[i - 1]
    }

    return(output)
  }

  if (method == "simple")
    return(sim(input, len))
  if (method == "median")
    return(med(input, len))
  if (method == "exponential")
    return(exp(input, len))
  return(0)
}
