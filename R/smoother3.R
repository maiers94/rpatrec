#'Perform Savitzgy-Golay smoothing on Time Series Data
#'
#'Use this function to smooth your time series data using local polynomial regression, as first pouplarised
#'by Savitzky and Golay (1964).
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'See the report for detailed references
#'
#'@param input Input Vector with Time Series Data
#'@param width Width of the filter (to each side of the centre)
#'@param degree Highest degree polynomial
#'
#'@return Vector containing smoothed time series data.
#'
#'@examples
#'savgolay(input=c(1,6,2,46,23,1,2,13,23,35,23,-2,3,23))
#'\dontrun{
#'#create a standard HS pattern:
#'a <- generator()
#'#add noise to this patterns
#'b <- noise(a,'white',10)
#'#smooth to regain the signal
#'c <- savgolay(b,8,2)
#'}
#'
#'
#'@export

savgolay <- function(input, width = 4, degree = 2) {
  
  data <- input
  
  inputchecks(list(data, width, degree), "savgolay")
  
  # perform the actual sg smoothing
  sg.filter <- function(input, N) {
    # N <= 2M
    M <- (length(input) - 1)/2
    # set up convolution coefficients
    A <- matrix(nrow = (2 * M + 1), ncol = (N + 1))
    col <- seq(from = -M, to = M, by = 1)
    
    for (i in 1:ncol(A)) {
      A[, i] <- col^(i - 1)
    }
    At <- t(A)
    H <- solve(At %*% A) %*% At
    
    # a <- H%*%input
    
    
    y <- vector(length = (2 * M + 1))
    for (j in 1:length(y)) {
      y[j] <- H[1, j] * input[j]
    }
    
    y0 <- sum(y)
    return(y0)
  }
  
  
  if (length(data)%%2 == 0) 
    data <- data[2:length(data)]
  steps <- length(data) - width * 2
  output <- vector()
  for (i in 1:steps) {
    output[i] <- sg.filter(data[i:(i + width * 2)], N = degree)
  }
  return(output)
}

