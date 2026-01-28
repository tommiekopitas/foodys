# Question 3(ii): Generating the random variables and plotting histograms


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



# Part (ii): Generate 10,000 values and Plot

N_sim <- 10000

# 1. Generate Data
samples_m1 <- sim_mech1_service(N_sim)
samples_m2 <- sim_mech2_service(N_sim)

# 2. Plot Histogram for Mechanic 1 (Rejection Method)
# We need to scale the theoretical density to match the histogram counts.
# Scaling Constant = Total Samples * Bin Width

# file for Mechanic 1
png("mechanic_1_histogram.png", width = 800, height = 600)

h1 <- hist(samples_m1, breaks = 50, plot = FALSE)
bin_width1 <- h1$breaks[2] - h1$breaks[1]
scaling_factor1 <- N_sim * bin_width1

hist(samples_m1, breaks = 50, main = "Mechanic 1: Rejection Method Simulation",
     xlab = "Service Time (x)", col = "lightblue", border = "white",
     ylim = c(0, 1000)) 

# Add Theoretical PDF Curve
x_vals1 <- seq(0, max(samples_m1), length.out = 100)
# f(x) multiplied by scaling factor
y_vals1 <- (0.5 * (1 + x_vals1) * exp(-x_vals1)) * scaling_factor1
lines(x_vals1, y_vals1, col = "red", lwd = 2)

# Legend for Mechanic 1 (Added to match style)
legend("topright", legend = c("Simulated", "Theoretical PDF"), 
       col = c("lightblue", "red"), lwd = c(NA, 2), pch = c(15, NA))

dev.off() # Close file for Mechanic 1


# 3. Plot Histogram for Mechanic 2 (Inverse Transform)

# file for Mechanic 2
png("mechanic_2_histogram.png", width = 800, height = 600)

h2 <- hist(samples_m2, breaks = 50, plot = FALSE)
bin_width2 <- h2$breaks[2] - h2$breaks[1]
scaling_factor2 <- N_sim * bin_width2

hist(samples_m2, breaks = 50, main = "Mechanic 2: Inverse Transform Simulation",
     xlab = "Service Time (x)", col = "lightgreen", border = "white",
     ylim = c(0, 1000))

# Add Theoretical PDF Curve
# Deriv of F(x) -> f(x) = 6x * exp(-3x^2)
x_vals2 <- seq(0, max(samples_m2), length.out = 100)
y_vals2 <- (6 * x_vals2 * exp(-3 * x_vals2^2)) * scaling_factor2
lines(x_vals2, y_vals2, col = "darkgreen", lwd = 2)

# Legend
legend("topright", legend = c("Simulated", "Theoretical PDF"), 
       col = c("lightgreen", "darkgreen"), lwd = c(NA, 2), pch = c(15, NA))

dev.off() # Close file for Mechanic 2

print("Plots saved to your folder.")