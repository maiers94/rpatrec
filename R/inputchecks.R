#this function provides input checks for other functions in this package. The idea is to have a centralised input check as this
#should produce more efficient code

#arg: vector of arguments
#who: name of the calling function
inputchecks <- function(arg,who){


  if(who=="generator"){
    #arg 1 needs to be numeric
    #arg 2,3 need to be numeric and greater than 0
    #arg 4,5 need be non-negative integers
    #arg 7 needs to be a vector
    #arg 8 needs to be a vector of length of arg 6
    singlenumber(arg[[1]])
    singlenumber(arg[[2]],pos=TRUE)
    singlenumber(arg[[3]],pos=TRUE)
    singlenumber(arg[[4]],nneg=TRUE,int=TRUE)
    singlenumber(arg[[5]],nneg=TRUE,int=TRUE)
    #singlenumber(arg[[6]])
    vectornumber(arg[[7]])
    vectornumber(arg[[8]],length(arg[[7]]))
  }
  if(who=="noise"){
    #arg1 needs to be a numeric vector
    #arg2 needs to be "white", "red"
    #arg3 needs to be numeric  positive
    vectornumber(arg[[1]])
    correctstring(arg[[2]],c("white","red","var"))
    singlenumber(arg[[3]],pos = TRUE)
  }
  if(who=="sample.pre"){
    if(!is.vector(arg[[1]]))stop("invalid arguments - argument needs to be a vector")
    if(is.na(arg[[1]][1]))stop("invalid arguments - vector must not start with NA")
  }
  if(who=="mav"){
    vectornumber(arg[[1]])
    singlenumber(arg[[2]],pos=TRUE,int=TRUE)
    correctstring(arg[[3]],specific=c("simple","median","exponential"))
  }
  if(who=="kernel"){
    vectornumber(arg[[1]])
    #arg[[2]] checked in function due to complexity
  }
  if(who=="savgolay"){
    vectornumber(arg[[1]])
    singlenumber(arg[[2]],pos=TRUE,int=TRUE)
    singlenumber(arg[[3]],pos=TRUE,int=TRUE)
  }
  if(who=="slicer"){
    vectornumber(arg[[1]])
    singlenumber(arg[[2]],pos=TRUE,int=TRUE)
    singlenumber(arg[[3]],pos=TRUE,int=TRUE)
    functioncheck(arg[[4]])
  }
  if(who=="interpret"){
    vectornumber(arg[[1]])
    functioncheck(arg[[2]])
  }

  return(0)
}

singlenumber <- function(x,int=FALSE,pos=FALSE,nneg=FALSE,specific=FALSE){
  if(!is.numeric(x))stop("invalid arguments - not numeric")
  if(length(x)!=1)stop("invalid arguments - not of length 1")
  if(int==TRUE){
    if(x%%1!=0)stop("invalid arguments - not an integer")
  }
  if(pos==TRUE){
    if(x<=0)stop("invalid arguments - not positive")
  }
  if(nneg==TRUE){
    if(x<0)stop("invalid arguments - not non-negative")
  }
  if(specific!=FALSE){
    if(x!=specific)stop("invalid arguments - element needs specific value")
  }
  return(0)
}

vectornumber <- function(x,l=FALSE){
  if(l!=FALSE){
    if(length(x)!=l)stop("invalid arguments - not of correct length")
  }
  if(!is.numeric(x))stop("invalid arguments - not numeric")
  return(0)
}

correctstring <- function(x,specific=FALSE){
  if(!is.character(x))stop("invalid arguments - needs to be a character")
  if(specific[1]!=FALSE){
    if(length(specific)==1){
      if(x!=specific)stop("invalid arguments - specified option not available")
    }
    else{
      check <- 0
      for(i in 1:length(specific)){
        if(x==specific[i])check <- 1
      }
      if(check==0)stop("invalid arguments - specified option not available")
    }
  }
  return(0)
}

functioncheck <- function(x){
    if(!is.function(x)){
      if(x!=FALSE)stop("invalid argument - needs to be a function or FALSE")
    }
    else if(length(formals(x))!=3)stop("invalid arguments - user defined function incorrect")

  return(0)
}


