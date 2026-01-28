# Question 3(i): Correct the distribution of the mechanics
# Simulation functions based on Lecture 2 methods, code is based on seminar 2 R. codes.

# Clear environment
rm(list = ls())
set.seed(1)


# Mechanic 1: Acceptance-Rejection Method
# Target f(x) = 0.5 * (1 + x) * exp(-x)
# Candidate g(x) = 0.5 * exp(-0.5x)  (Exponential with rate 0.5)
sim_mech1_service <- function(n) {
  
  # Initialize the vector to store generated values
  X_values <- numeric(n)
  
  # Calculate constant c (as derived in the report)
  # c is the max of f(x)/g(x)
  c_val <- 2 * exp(-0.5)
  
  # Loop to generate n samples
  for (k in 1:n) {
    
    flag <- 1
    
    
    while(flag) {
      
      # Generate Y from candidate density g(x) (Exp(0.5))
      # Using Inverse Transform for Exponential: -log(U) / lambda
      U1 <- runif(1)
      Y <- -log(U1) / 0.5
      
      # Generate Uniform U2 for the acceptance test
      U2 <- runif(1)
      
      # Calculate the acceptance probability ratio K
      # f(Y) = 0.5 * (1 + Y) * exp(-Y)
      # g(Y) = 0.5 * exp(-0.5 * Y)
      # Ratio f/g = (1 + Y) * exp(-0.5 * Y)
      
      ratio <- (1 + Y) * exp(-0.5 * Y)
      K <- ratio / c_val
      
      # Check if condition is met
      if (U2 <= K) {
        X_values[k] <- Y  # Accept Y as our random variable X
        flag <- 0         # Stop the while loop
      }
    }
  }
  
  return(X_values)
}

# Mechanic 2: Inverse Transform Method
# CDF F(x) = 1 - exp(-3 * x^2)
# Inverse derived: x = sqrt( -ln(1-u) / 3 )

sim_mech2_service <- function(n) {
  
  # Initialize vector
  X_values <- numeric(n)
  
  # Generate n uniform random variables
  U <- runif(n)
  
  # Apply the inverse CDF formula
  # Note: (1-U) is distributed same as U, but we use 1-U based on algebra
  X_values <- sqrt(-log(1 - U) / 3)
  
  return(X_values)
}