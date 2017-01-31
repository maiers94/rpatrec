#'@importFrom graphics plot

slicer <- function(data,length,step=1){
  no.windows <- (length(data)-length)/step+1
  output <- vector(length=no.windows)
  for(i in 1:no.windows){
    lower <- (i-1)*(step)+1
    upper <- lower + length-1
    window <- data[lower:upper]
    output[i] <- interpret(window)
    if(output[i]!=0)plot(window)

  }
  return(output)
}


interpret <- function(window){

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
  iq <- function(ext){
    pattern <- 0
    if(length(ext)<5)return(0)
    for(i in 5:length(ext)){
      if(withinavg(exvals[i-3],exvals[i-1]) == TRUE){
        if(ext[i-4]==1){
          if(exvals[i-2]>exvals[i-4]&&exvals[i-2]>exvals[i])return(c("HS",exvals[(i-4):i]))
        }
        if(ext[i-4]==0){
          print(ext[i-4])
          if(exvals[i-2]<exvals[i-4]&&exvals[i-2]<exvals[i])return(c("Inv HS",exvals[(i-4):i]))
        }
      }
    }
    return(0)
  }
  #########
  withinavg <- function(a,b,p=0.1){
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
  result <- iq(extrema)
  print(extrema)
  return(result)
}
