#'Prepare Data for using with the other functions in the package
#'
#'This function removes bank holidays and other days for which only \code{NA} or repeated data is available.
#'Designed to work with Datastream data.
#'
#'For an overview of the package capabilities, click here \link{rpatrec}.
#'
#'@param input Vector with time series data
#'
#'@return Returns a vector conataining time series data ready for further use.
#'@export

sample.pre <- function(input) {
  inputchecks(list(input), "sample.pre")
  output <- input[1]
  cut <- 0
  for (i in 2:length(input)) {
    if (!is.na(input[i])) {
      if (input[i - 1] != input[i]) {
        output <- c(output, input[i])
      }
      else cut <- cut + 1
    }
    else {
      cut <- cut + 1
      input[i] <- input[i - 1]
    }

  }
  print(c("cut input by", cut))
  return(output)
}
