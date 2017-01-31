#library(np)

kernel <- function(input, bandwidth="auto"){
  x <- seq(1,length(input))

  if(is.numeric(bandwidth))reg <- np::npreg(input ~ x, ckertype = "gaussian", bws = bandwidth, regtype="lc")
  if(bandwidth=="auto")reg <- np::npreg(input ~ x, ckertype = "gaussian", bwmethod="cv.ls", regtype="lc")

  output <- fitted(reg)
  print(reg[[1]])
  return(output)
}
