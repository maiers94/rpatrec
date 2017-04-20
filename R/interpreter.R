#'Recognise Multiple Patterns in a sinlge  time series
#'
#'Break time series data into smaller "windows" and pass them to the \link{interpret} function.
#'The results are summarised in the output. For details, run \link{interpret} on specific windows only.
#'
#'For an overview of the package capabilities, click here \link{rpatrec}
#'
#'@param data Time series Data
#'@param length Length of the "windows"
#'@param step Number of Data Points between windows
#'@param useriq User-built recognition function. Set to \code{FALSE} if using inbuilt recognition capabilities.
#'    Refer to the readme or the report on how to build your own recognition function
#'@param ... Parameters passed on to either the inbuilt or external recognition function. Check \link{iq} for the parameters.

#'
#'@return A list containing: \itemize{
#'  \item{A vector for every window analysed showing 0 if no pattern and 1 if at least 1 pattern has been found.}
#'  \item{A vector with the starting index of those windows where a pattern has been found.}
#' }
#'
#'
#'@examples
#'\dontrun{
#'#Generate 2 HS patterns
#'a <- c(generator(),generator())
#'#recognise both HS patterns
#'#set window size to 100, step size to 100
#'#switch off recognition for all patterns other than HS
#'slicer(data = a, length = 100, step = 100, hsiq=TRUE, btpiq=FALSE, rtpiq=FALSE, dtpiq=FALSE)
#'}
#'
#'@export
#'
#'
#'

slicer <- function(data,length,step=1,useriq=FALSE,...){

  inputchecks(list(data,length,step,useriq),"slicer")

  no.windows <- ceiling((length(data)-length)/step)+1
  output <- vector(length=no.windows)
  for(i in 1:no.windows){
    lower <- (i-1)*(step)+1
    upper <- lower + length-1
    if(upper > length(data)){
      upper <- length(data)
    }
    window <- data[lower:upper]
    cur <- interpret(window,useriq,...)
    if(cur$RESULT==TRUE)output[i] <- 1
    else output[i] <- 0
  }
  pct <- round(length(output[output==1])/length(output)*100)
  print(c("Patterns were found in ", pct,"% of windows analysed. Refer to function value for details"))
  posw <- (which(output!=0)-1)*step
  return(list(output,posw))
}

#'Recognise patterns in Time Series Data
#'
#'Use this function to either check for the inbuilt financial markets pattern or to use your own
#'recognition function as described in the readme.
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'
#'@param window Time Series Data
#'@param useriq User-built recognition function. Set to \code{FALSE} if using inbuilt recognition capabilities.
#'    Refer to the readme or the report on how to build your own recognition function.
#'@param ... Parameters passed on to either the inbuilt or external recognition function. Check \link{iq} for the parameters of the internal function.
#'
#'@return A list containing the following: \itemize{
#'\item{"EXT"}{ All extrema found in the sample, 0 for minima and 1 for maxima}
#'\item{"EXP"}{ Value of these extrema (y-coordinate)}
#'\item{"EXP"}{ Position of these extrema (x-coordinate)}
#'\item{Recognition Output}{ A list containing the extrema that form part of the pattern labelled by either the custom or \link{iq} function.\itemize{
#'\item{"HSP"}{ Can be either: \itemize{
#'\item{"HS"}{ (Head and Shoulders)}
#'\item{"InvHS"}{ (Inverse Head and Shoulders)}
#'}}
#'\item{"BTPorTTP"}{ Can be either: \itemize{
#'\item{"BTOP"}{ (Broadening Top)}
#'\item{"BBOT"}{ (Broadening Bottom)}
#'\item{"TTOP"}{ (Triangle Top)}
#'\item{"TBOT"}{ (Triangle Bottom)}
#'}}
#'\item{"RTP"}{ Can be either: \itemize{
#'\item{"RTOP"}{ (Rectangle Top)}
#'\item{"RBOT"}{ (Rectangle Bottom)}
#'}}
#'\item{"DTP"}{ Can be either: \itemize{
#'\item{"DTOP"}{ (Double Top)}
#'\item{"DBOT"}{ (Double Bottom)}
#'}}
#'}}
#'\item{"RESULT"}{ \code{TRUE} if any pattern is found, \code{FALSE} otherwise}
#'}
#'
#'@examples
#'\dontrun{
#'#Generate HS patterns
#'a <- generator()
#'#switch off recognition for all patterns other than HS
#'interpret(window = a, useriq=FALSE, hsiq=TRUE, btpiq=FALSE, rtpiq=FALSE, dtpiq=FALSE)
#'}
#'
#'@export
#'

interpret <- function(window,useriq=FALSE,...){

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
    result <- useriq(extrema,exvals,expos,...)
  }
  else result <- iq(extrema,exvals,expos,...)
  #print(extrema)
  return(result)
}


#'Inbuilt Recoqnition for 10 different financial markets patterns
#'
#'Do not call individually. Switch recognition on/off for certain patterns, use the arguments in the \link{interpret} or \link{slicer} function.
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'
#'@param hsiq Logical. Recognise (inverse) Head and Shoulders pattern
#'@param btpiq Logical. Recognise Triangle and/or Broadening tops and bottoms pattern
#'@param rtpiq Logical. Recognise Rectangle tops and bottoms pattern
#'@param dtpiq Logical. Recognise Double tops and bottoms pattern
#'
#'@param ext Extrema
#'@param exvals Values of the extrema
#'@param expos Position of the extrema
#'

iq <- function(ext,exvals,expos,hsiq=TRUE,btpiq=TRUE,rtpiq=TRUE,dtpiq=TRUE){

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

  #############
  #- check for Head and shoulder and inverse head and shoulders
  iqhs <- function(ext,exvals){
    #pattern length needs to be 5 (or more if other data follows)
    if(length(ext)<5)return(NA)
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
    return(NA)
  }
  ##############
  #- check for Broadening tops and bottoms and triangle tops and bottoms
  iqbtp <- function(ext,exvals){
    #pattern length needs to be 5 (or more if other data follows)
    if(length(ext)<5)return(NA)
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
    return(NA)
  }
  ##############
  #- check for rectangle tops and bottoms
  iqrtp <- function(ext,exvals){
    #pattern length needs to be 5 (or more if other data follows)
    if(length(ext)<5)return(NA)
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
    return(NA)
  }
  ##############
  #- check for double tops/bottoms
  iqdtp <- function(ext,exvals,expos){
    #pattern length needs to be 3 (or more if other data follows)
    if(length(ext)<3)return(NA)

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
    return(NA)
  }
  ##############

  if(hsiq==TRUE)HS <- iqhs(ext,exvals)
  else HS <- NA
  if(btpiq==TRUE)BTP <- iqbtp(ext,exvals)
  else BTP <- NA
  if(rtpiq==TRUE)RTP <- iqrtp(ext,exvals)
  else RTP <- NA
  if(dtpiq==TRUE)DTP <- iqdtp(ext,exvals,expos)
  else DTP <- NA

  if(is.na(HS)&&is.na(BTP)&&is.na(RTP)&&is.na(DTP))res <- FALSE
  else res <- TRUE
  pattern <- list(EXT=ext,EXV=exvals,EXP=expos,HSP=HS,BTPorTTP=BTP,RTP=RTP,DTP=DTP,RESULT=res)

  return(pattern)
}




