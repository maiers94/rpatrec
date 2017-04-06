#library(openxlsx)


# data <- read.xlsx("ds_new.xlsx",2)
# milan <- data[[1]]
# sandp.tsx <- data[[2]]
# dax <- data[[3]]
# dow <- data[[4]]
# nikkei <-data[[5]]

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

