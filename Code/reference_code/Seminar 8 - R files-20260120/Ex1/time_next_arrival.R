# This function takes as input time s, constant lambda, and outputs the time 
# of the first (next) arrival after time s.

# Note that it calls the function lambdaf, which is the function lambda(t) - 
# the Poisson rate over time. We define lambdaf in another file.

source("C:/Users/PAPADAKK/Dropbox/MA424 - New/Lecture 8/L8 seminar/R code/lambdaf.R")


time_next_arrival = function(s, lambda){
  t = s
  flag = 1

  while(flag){
    U1 = runif(1)
    t = t - (1/lambda) * log(U1)
    
    U2 = runif(1)
    
    if (U2 <= lambdaf(t)/lambda){
      T_s = t
      flag = 0
    }
  }
  
  return(T_s)
}