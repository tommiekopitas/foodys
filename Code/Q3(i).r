#========================
# Question 3
# Simulation of service times for two mechanics using different methods
# Based on Lecture 2 methods, code structure follows seminar 2 R codes
#========================

# Clear environment and set seed for reproducibility
rm(list = ls())
set.seed(1)

#========================
# Question 3(i): Functions to simulate service times for each mechanic
#========================


# Mechanic 1: Acceptance-Rejection Method
#
# We use the Acceptance-Rejection method because the target density 
# f(x) = 0.5 * (1 + x) * exp(-x) does not have a closed-form inverse CDF,
# making the Inverse Transform method impractical. The Acceptance-Rejection
# method allows us to sample from f(x) by using a simpler candidate density
# g(x) that we can easily sample from.
#
# Target density: f(x) = 0.5 * (1 + x) * exp(-x), for 0 < x < infinity
# Candidate density: g(x) = 0.5 * exp(-0.5x) (Exponential with rate 0.5)
#
# The constant c is chosen as the supremum of f(x)/g(x), which we derived
# analytically in the report as c = 2 * exp(-0.5)


sim_mech1_service <- function(n) {
  
  # Initialize the vector to store generated values
  X_values <- numeric(n)
  
  # Calculate constant c (as derived in the report)
  # c is the supremum of f(x)/g(x) over all x > 0
  c_val <- 2 * exp(-0.5)
  
  # Loop to generate n samples using Acceptance-Rejection
  for (k in 1:n) {
    
    flag <- 1 # flag to control the while loop (1 = keep trying, 0 = accepted)
    
    while(flag) {
      
      # Step 1: Generate Y from candidate density g(x) = 0.5 * exp(-0.5x)
      # Using Inverse Transform for Exponential: Y = -log(U) / lambda
      U1 <- runif(1)
      Y <- -log(U1) / 0.5
      
      # Step 2: Generate independent Uniform U2 for the acceptance test
      U2 <- runif(1)
      
      # Step 3: Calculate the acceptance probability ratio
      # The ratio f(Y)/g(Y) simplifies to (1 + Y) * exp(-0.5 * Y)
      # We then divide by c to get K, the acceptance probability
      ratio <- (1 + Y) * exp(-0.5 * Y)
      K <- ratio / c_val
      
      # Step 4: Accept Y if U2 <= K, otherwise reject and repeat
      if (U2 <= K) {
        X_values[k] <- Y  # Accept Y as our random variable X
        flag <- 0         # Exit the while loop
      }
    }
  }
  
  return(X_values)
}


# Mechanic 2: Inverse Transform Method
#
# We use the Inverse Transform method because the CDF F(x) = 1 - exp(-3x^2)
# can be inverted analytically to obtain a closed-form expression for F^{-1}(u).
# This makes the Inverse Transform method the natural and efficient choice.
#
# CDF: F(x) = 1 - exp(-alpha * x^beta), where alpha = 3 and beta = 2
# Solving u = F(x) for x gives: x = sqrt(-log(1-u) / 3)
#
# Note: Since U ~ Uniform(0,1), we have (1-U) ~ Uniform(0,1) as well,
# but we use (1-U) to match our algebraic derivation exactly.


sim_mech2_service <- function(n) {
  
  # Initialize vector to store generated values
  X_values <- numeric(n)
  
  # Generate n uniform random variables on (0,1)
  U <- runif(n)
  
  # Apply the inverse CDF formula: F^{-1}(u) = sqrt(-log(1-u) / 3)
  X_values <- sqrt(-log(1 - U) / 3)
  
  return(X_values)
}