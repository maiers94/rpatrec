

#'Prepare Data for using with the other functions
#'
#'@param input Vector with timeseries data
#'
#'@return Returns a vector conataining time series data ready forfurther use by removing non-numeric elements, and removing repeated values.
#'@export

sample.pre <- function(input){
  inputchecks(list(input),"sample.pre")
  output <- input[1]
  cut <- 0
  for(i in 2:length(input)){
    if(!is.na(input[i])){
      if(input[i-1]!=input[i]){
        output <- c(output,input[i])
      }
      else cut <- cut + 1
    }
    else{
      cut <- cut +  1
      input[i]<- input[i-1]
    }

  }
  print(c("cut input by",cut))
  return(output)
}

