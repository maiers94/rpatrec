#
#'Compute the moving average, exponential average or running median.
#'
#'@param input Time series data to use for computation
#'@param len An integer to determine the number of datapoints used for computation
#'@param method String. Determines the method of computation.
#'    Permissible values are \code{exponential}, \code{simple} and \code{median}
#'
#'@return Vector containing computed values of length of \code{input} less length of \code{len}
#'
#'@examples
#'mav(1:10,5,"exponential")
#'
#'@export
#'@importFrom stats median
#'


mav <- function(input,len = 10,method){

  inputchecks(list(input,len,method),"mav")

  #########
  sim <- function(inp,l){
    output <- vector(length = (length(inp)-l))
    for(i in 1:(length(inp)-l)){
      output[i] <- sum(inp[i:(i+len)-1])/l
          }
    return(output)
  }
  ##########
  med <- function(inp,l){
    output <- vector(length = (length(inp)-l))
    for(i in 1:(length(inp)-l)){
      output[i] <- median(inp[i:(i+len)])
    }
    return(output)
  }
  ##########
  exp <- function(inp,l){
    output <- vector(length = (length(inp)-l))
    output[1] <- sum(inp[1:l])/l
    alpha <- 2/(l+1)
    for(i in 2:(length(inp)-l)){
      cur <- output[i-1]
      output[i] <- (inp[i+l]-output[i-1])*alpha + output[i-1]
    }

    return(output)
  }

  if(method=="simple")return(sim(input,len))
  if(method=="median")return(med(input,len))
  if(method=="exponential")return(exp(input,len))
  return(0)
}
