#'Test how well a smoother can filter noise from data
#'
#'@param n number of runs
#'@param m number of runs per level of noise
#'@param incr value by which the error is increased in each turn
#'@param max max number of times the error is increased
#'@param smoother Function with pre-defined inputs, so that only the parameter \code{input} is left to be defined
#'@param ... other parameters the smoother requires
#'
#'@export
test.smoother <- function(n=1,m=5,incr=1,max=100,smoother,...){
  result <- vector()
  for(i in 1:n){
    pat <- generator()
    r <- 1
    zero <- 0
    nresult <- vector()
    while(r <= max && zero < 3){
      curerror <- (r - 1) * incr
      count <- 0
      for(j in 1:m){
        epat <- noise(pat,"white",curerror)
        spat <- smoother(input = epat,...)
        reci <- interpret(spat)
        rec <- reci[[4]][[1]]
        #### if checks
        #check whether the pattern has been recognised
        if(rec[1] > 50*0.95 && cur[1] < 50 *1.05){
          if(rec[2] > 25*0.95 && cur[2] < 25 *1.05){
            if(rec[3] > 100*0.95 && cur[3] < 100 *1.05){
              if(rec[4] > 25*0.95 && cur[4] < 25 *1.05){
                if(rec[5] > 50*0.95 && cur[5] < 50 *1.05){
                  status <- TRUE
                }
              }
            }
          }
        }
        else status <- FALSE
        #### end if checks
        if(status == TRUE)count <- count + 1
      }
      nresult[r] <- count / m * 100
      r <- r + 1
    }
    result <- result  + nresult
  }

  result <- result / n

  return(result)
}


