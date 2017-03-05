#'Break time series data into smaller "windows" and pass them to the interpret() function
#'
#'@param data Time series Data
#'@param length Length of the "windows"
#'@param step Number of Data Points between windows
#'@param useriq User-built recognition function. Set to \code{FALSE} if using inbuilt recognition capabilities.
#'    This function must take a vector of 0s and 1s as first input and a vector of the extremum values as second input.
#'    It should return the desired result.
#'
#'
#'@importFrom graphics plot
#'@export
#'
#'
#'

slicer <- function(data,length,step=1,useriq=FALSE){

  inputchecks(list(data,length,step,useriq),"slicer")

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
#'
#'
#'@export
#'

interpret <- function(window,useriq=FALSE){

  inputchecks(list(window,useriq),"interpret")

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

  iq <- function(ext,exvals,expos){

    #############
    #- check for Head and shoulder and inverse head and shoulders
    iqhs <- function(ext,exvals){
      #pattern length needs to be 5 (or more if other data follows)
      if(length(ext)<5)return(0)
      for(i in 5:length(ext)){
        #2nd & 4th extremum must be within 10% average
        if(withinavg(c(exvals[i-3],exvals[i-1]),p=0.1) == TRUE){
          #if the first extremum is a maximum, it is a Head and shoulders pattern
          if(ext[i-4]==1){
            if(exvals[i-2]>exvals[i-4]&&exvals[i-2]>exvals[i])return(list(HS=exvals[(i-4):i]))
          }
          if(ext[i-4]==0){
            #if the first extremum is a minium, it is an inverse Head and shoulders pattern
            if(exvals[i-2]<exvals[i-4]&&exvals[i-2]<exvals[i])return(list(InHS=exvals[(i-4):i]))
          }
        }
      }
      return(0)
    }
    ##############
    #- check for Broadening tops and bottoms and triangle tops and bottoms
    iqbtp <- function(ext,exvals){
      #pattern length needs to be 5 (or more if other data follows)
      if(length(ext)<5)return(0)
      for(i in 5:length(ext)){
        #if 2nd is bigger than 4th
        if(exvals[i-3]>exvals[i-1]){
          #if 1st is smaller than 3rd and 3rd is smaller than 5th
          if(exvals[i-4]<exvals[i-2]){
            if(exvals[i-2]<exvals[i]){
              #if first is max
              if(ext[i-4]==1)return(list(BTOP=exvals[(i-4):i]))
              #if first is min
              if(ext[i-4]==0)return(list(TBOT=exvals[(i-4):i]))
            }
          }
        }
        #if 2nd is smaller than 4th
        if(exvals[i-3]<exvals[i-1]){
          #if 1st is larger than 3rd and 3rd is larger than 5th
          if(exvals[i-4]>exvals[i-2]){
            if(exvals[i-2]>exvals[i]){
              #if first is min
              if(ext[i-4]==0)return(list(BBOT=exvals[(i-4):i]))
              #if first is max
              if(ext[i-4]==1)return(list(TTOP=exvals[(i-4):i]))
            }
          }
        }
      }
      return(0)
    }
    ##############
    #- check for rectangle tops and bottoms
    iqrtp <- function(ext,exvals){
      #pattern length needs to be 5 (or more if other data follows)
      if(length(ext)<5)return(0)
      for(i in 5:length(ext)){
        #tops are within 1% of their average
        if(withinavg(exvals[ext==1],0.01)){
          if(withinavg(exvals[ext==0],0.01)){
            if(min(exvals[ext==1])>max(exvals[ext==0])){
              if(ext[i-4]==0)return(list(RBOT=exvals[(i-4):i]))
              if(ext[i-4]==1)return(list(RTOP=exvals[(i-4):i]))
            }
          }
        }
      }
      return(0)
    }
    ##############
    #- check for double tops/bottoms
    iqdtp <- function(ext,exvals,expos){
      #pattern length needs to be 3 (or more if other data follows)
      if(length(ext)<3)return(0)

      for(i in 3:length(ext)){
        rext <- ext[(i-1):length(ext)]
        rexvals <- exvals[(i-1):length(exvals)]
        rexpos <- expos[(i-1):length(exvals)]
        if(ext[i-2]==1){
          maxpos <- which.max(rexvals)
          if(rext[maxpos]==1){
            if(withinavg(c(exvals[i-2],rexvals[maxpos]),0.05)){
              #at least 22 trading days apart
              if(rexpos[maxpos]-expos[i-2]>22)return(list(DTOP=exvals[(i-2):i]))
            }
          }
        }
        if(ext[i-2]==0){
          minpos <- which.min(rexvals)
          if(rext[minpos]==0){
            if(withinavg(c(exvals[i-2],rexvals[minpos]),0.05)){
              #at least 22 trading days apart
              if(rexpos[minpos]-expos[i-2]>22)return(list(DBOT=exvals[(i-2):i]))
            }
          }
        }
      }
      return(0)
    }
    ##############

    HS <- iqhs(ext,exvals)
    BTP <- iqbtp(ext,exvals)
    RTP <- iqrtp(ext,exvals)
    DTP <- iqdtp(ext,exvals,expos)

    pattern <- list(EXT=ext,EXV=exvals,EXP=expos,HSP=HS,BTPorTTP=BTP,RTP=RTP,DTP=DTP)

    return(pattern)
  }

  #########
  withinavg.old <- function(a,b,p){
    a <- abs(a)
    b <- abs(b)
    c <- (a+b)/2
    if(a>(1-p)*c && a<(1+p)*c && b>(1-p)*c && b<(1+p)*c)return(TRUE)
    else return(FALSE)
  }

  withinavg <- function(x,p){

    c <- sum(x)/length(x)
    c <- abs(c)
    x <- abs(x)
    for(i in 1:length(x)){
      if(x[i]<(1-p)*c || x[i]>(1+p)*c)return(FALSE)
    }
    return(TRUE)
  }
  #########

  extrema <- vector()
  exvals <- vector()
  expos <- vector()
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
        expos <- c(expos,(i-1))
      }
      if(trend == "down" && y == 1){
        extrema <- c(extrema,0)
        trend <- "up"
        exvals <- c(exvals,window[i-1])
        expos <- c(expos,(i-1))
      }
    }
  }
  if(is.function(useriq)){
    result <- useriq(extrema,exvals,expos)
  }
  else result <- iq(extrema,exvals,expos)
  #print(extrema)
  return(result)
}



