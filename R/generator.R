#'Generate a time series containing a Visual Charting Pattern.
#'
#'@param start Starting value
#'@param dlength Integer. Length of the signal part of the time series
#'@param tot.spread Integer. Difference between the lowest and highest value of the time series
#'@param presig Integer. Length of the pre-signal part of the time series
#'@param postsig Integer. Length of the post-signal part of the time series
#'@param plength Integer. Number of extrema in the pattern
#'@param parts Vector of Integers. Vector must contain plength + 2 elements, the first element being 0 and the last 100.
#'    defines how far the extrema lie apart from another (in percent)
#'@param sprd Vector of Integers.Vector must contain plength + 2 elements, the first and last elemnt should be 0.
#'    Defines the value of the extrema in percent of \code{tot.spread} in relation to \code{start}
#'
#'@return Time series with (optional) pre- or post signal, and the specified pattern.
#'
#'@examples
#'#create a standard HS pattern:
#'generator()
#'#create a shifted head and shoulders pattern
#'generator(sprd = c(0,20,10,90,40,60,0))
#'
#'@export
#'
#'
generator <- function(start = 0, dlength = 100, tot.spread = 100, presig = 0, postsig = 0, plength = 5, parts = c(0,15,25,50,75,85,100), sprd = c(0,50,25,100,25,50,0)){
  #generate any pattern


  start.const <- start
  neg <- start - tot.spread
  negative <- FALSE
  if(neg <= 0){
    start <- start - neg +10
    negative <- TRUE
  }
  start.const <- start

  #make data
  output <- vector(length = dlength)

  partitions <- as.integer(round(parts/100*dlength))
  pre_spreads <- sprd/100         ##same
  for(i in 1:(plength+1)){
    reference <- start.const + round(tot.spread*pre_spreads[i+1])

    curspread <- 2*tot.spread*abs(pre_spreads[i+1]-pre_spreads[i])/(partitions[i+1]-partitions[i])
    #print(curspread)
    output[(partitions[i]+1):partitions[i+1]] <- sectgen((partitions[i+1]-partitions[i]),start,reference,curspread)
    start <- output[(partitions[i+1]-1)]

  }
  if(negative==TRUE)output <- output + neg -10
  if(presig != 0){
    pre <- vector(length=presig)
    pre <- rep(output[1],presig)
    output <- c(pre,output)
  }
  if(postsig != 0){
    post <- vector(length=postsig)
    post <- rep(output[length(output)],postsig)
    output <- c(output,post)
  }
  return(output)
}

#'@importFrom stats runif

sectgen <- function(sectlen,init,ref,spread,acc = 0.0001){
  sector <- vector(length = sectlen)
  expected <- ref
  print(ref)
  print(init)

  expmin <- expected * (1-acc)
  expmax <- expected * (1+acc)
  repeat{
    cur <- init
    for(j in 1:sectlen){
      sector[j] <- cur
      if(ref<init)cur <- runif(1,sector[j]-spread,sector[j])
      if(ref>init)cur <- runif(1,sector[j],spread + sector[j])
    }
    if(sector[sectlen] > expmin){
      if(sector[sectlen] < expmax)break
    }
  }


  return(sector)
}

#'
#'Add noise to a time series
#'
#'@param input Time series to which noise will be added
#'@param type String. Defines what type of noise to be added. Acceptable values are:
#'    \code{white} - white noise with variance \code{level}
#'    \code{red} - red noise with variance \code{level} (use with caution)
#'@param level Number or \code{var}.
#'    A number sets the standard deviation to a constant value at each point.
#'    \code{var} sets the variance to increase proportional to the (absolute value of the) signal at peaks.
#'
#'
#'@return Time series with added noise.
#'
#'@export
#'@importFrom stats rnorm
#'

noise <- function(input,type,level){
  if(is.numeric(level))final_level <- level
  if(type=="var"){
    #set scale
    up <- max(abs(input))
    output <- vector(length=length(input))
    #create noise
    for(i in 1:length(input)){
      output[i] <- input[i] + rnorm(1,0,(abs(input[i])/up*final_level))
    }
  }
  if(type=="white"){
    output <- input + rnorm(length(input),0,final_level)
  }
  if(type=="red"){
    noise <- cumsum(rnorm(length(input),0,final_level))
    output <- input + noise
  }

  return(output)
}
