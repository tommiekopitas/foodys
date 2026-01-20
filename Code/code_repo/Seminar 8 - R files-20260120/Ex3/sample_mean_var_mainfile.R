# Clear your environment of variables
rm(list = ls())

# This code generates standard normal random variables until we have generated n 
# of them, where n <= 100 is such that S/sqrt{n} < 0.1, where S is the 
# sample standard deviation of the n data values.

# We use gen_normal(mu,sigma) function that generates normal rvs
source("C:/Users/PAPADAKK/Dropbox/MA424 - New/Lecture 8/L8 seminar/R code/gen_normal.R")

set.seed(1)
# We use the recursive formulas to calculate the sample mean and sample variance
n = 1 # n is the number of iterations
Z = gen_normal(0,1) # standard normal rv generated at current iteration
Z_bar = Z # Z_bar is the sample mean at iteration n
S_sq = 0 # S_sq is the sample variance at iteration n

# I wrote a function that takes as input n, the new realization Z(n+1), 
# the current Z_bar(n) and S_sq(n) and outputs the updated Z_bar(n+1) and 
# S_sq(n+1). I source it here:
source("C:/Users/PAPADAKK/Dropbox/MA424 - New/Lecture 8/L8 seminar/R code/gen_sample_mean_var.R")

# print(sqrt(S_sq/n))

while ((n < 100) | sqrt(S_sq/n) > 0.1){
  
  Z = gen_normal(0,1)
  out = gen_sample_mean_var(n,Z,Z_bar,S_sq)
  n = n + 1
  Z_bar = out[1]
  S_sq = out[2]
  #print(sqrt(S_sq/n))

  
}
  
print(n)

## part (e)
print(paste("Z_bar: the current sample mean  is ", round(Z_bar, digits = 4)))
print(paste("S_sq: the current sample variance is ", round(S_sq, digits = 4)))
print(paste("The s.d. of the estimator is ", round(sqrt(S_sq/n), digits = 4)))
  