#'Break time series data into smaller "windows" and pass them to the interpret() function
#'
#'@param data Time series Data
#'@param length Length of the "windows"
#'@param step Number of Data Points between windows
#'@param useriq User-built recognition function. Set to \code{FALSE} if using inbuilt recognition capabilities.
#'    This function must take a vector of 0s and 1s as first input and a vector of the extremum values as second input.
#'    it should return the desired result.
#'
#'
#'@importFrom graphics plot
#'
#'
#'
#'

slicer <- function(data,length,step=1,useriq=FALSE){
  no.windows <- (length(data)-length)/step+1
  output <- vector(length=no.windows)
  for(i in 1:no.windows){
    lower <- (i-1)*(step)+1
    upper <- lower + length-1
    window <- data[lower:upper]
    output[i] <- interpret(window,useriq)
    if(output[i]!=0)plot(window)

  }
  return(output)
}

#'Recognise patterns in Time Series Data
#'
#'@param window Time Series Data
#'@param useriq User-built recognition function. Set to \code{FALSE} if using inbuilt recognition capabilities.
#'    This function must take a vector of 0s and 1s as first input and a vector of the extremum values as second input.
#'    it should return the desired result.
#'

interpret <- function(window,useriq=FALSE){

  init.trend <- function(data){
    if(data[1]>data[2])t<-"down"
    if(data[1]<=data[2])t<-"up"
    return(t)
  }

  ########
  bors <- function(a,b){
    if(a==b) big <- 0 #####
    if(a>b) big <- -1
    if(a<b) big <- 1
    return(big)
  }
  ########
  #pattern recognition happens here, everything else is preparation

  iq <- function(ext,exvals){
    pattern <- 0

    #############
    ##### - check for Head and shoulder and inverse head and shoulders
    iqhs <- function(ext,exvals){
      #pattern length needs to be 5 (or more if other data follows)
      if(length(ext)<5)return(0)
      for(i in 5:length(ext)){
        #2nd & 4th extremum must be within 10% average
        if(withinavg(exvals[i-3],exvals[i-1],p=0.1) == TRUE){
          #if the first extremum is a maximum, it is a Head and shoulders pattern
          if(ext[i-4]==1){
            if(exvals[i-2]>exvals[i-4]&&exvals[i-2]>exvals[i])return(c("HS",exvals[(i-4):i]))
          }
          if(ext[i-4]==0){
            #if the first extremum is a minium, it is an inverse Head and shoulders pattern
            if(exvals[i-2]<exvals[i-4]&&exvals[i-2]<exvals[i])return(c("Inv HS",exvals[(i-4):i]))
          }
        }
      }
      return(0)
    }
    ##############



    #pattern length needs to be 5 (or more if other data follows)
    return(iqhs(ext,exvals))
    return(0)
  }

  #########
  withinavg <- function(a,b,p){
    a <- abs(a)
    b <- abs(b)
    c <- (a+b)/2
    if(a>(1-p)*c && a<(1+p)*c && b>(1-p)*c && b<(1+p)*c)return(TRUE)
    else return(FALSE)
  }
  #########

  extrema <- vector()
  exvals <- vector()
  setcheck <- FALSE
  trend<-init.trend(window)

  for(i in 3:length(window)){
    #print(window[i-1])
    x <- bors(window[i-2],window[i-1])
    y <- bors(window[i-1],window[i])

    if(x != y){
      if(trend == "up" && y == -1){
        extrema <- c(extrema,1)
        trend <- "down"
        exvals <- c(exvals,window[i-1])
      }
      if(trend == "down" && y == 1){
        extrema <- c(extrema,0)
        trend <- "up"
        exvals <- c(exvals,window[i-1])
      }
    }
  }
  if(is.function(useriq)){
    result <- useriq(extrema,exvals)
  }
  else result <- iq(extrema,exvals)
  print(extrema)
  return(result)
}
