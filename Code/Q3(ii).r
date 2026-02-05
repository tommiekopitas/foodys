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


#========================
# Question 3(ii): Generate 10,000 values and plot histograms with theoretical PDFs
#
# To verify that our simulation functions are correct, we generate a large sample
# from each distribution and compare the histogram to the theoretical density.
# The theoretical density must be scaled to align with the histogram counts.
#
# Scaling constant derivation:
# A histogram displays counts (frequencies) in each bin. The area under the 
# histogram equals the total number of samples (N_sim). The area under a PDF
# equals 1. To make them comparable, we multiply the PDF by:
#   Scaling Factor = N_sim * bin_width
# This ensures the area under the scaled PDF equals the histogram area.
#========================

N_sim <- 10000 # number of samples to generate

# Generate samples from both mechanics

samples_m1 <- sim_mech1_service(N_sim) # Mechanic 1 service times
samples_m2 <- sim_mech2_service(N_sim) # Mechanic 2 service times

#========================
# Plot Histogram for Mechanic 1 (Acceptance-Rejection Method)
#========================

# Save plot to PNG file
png("mechanic_1_histogram.png", width = 800, height = 600)

# First compute histogram without plotting to extract bin width
h1 <- hist(samples_m1, breaks = 50, plot = FALSE)
bin_width1 <- h1$breaks[2] - h1$breaks[1]

# Calculate scaling factor: N_sim * bin_width
scaling_factor1 <- N_sim * bin_width1

# Plot the histogram
hist(samples_m1, breaks = 50, main = "Mechanic 1: Acceptance-Rejection Method Simulation",
     xlab = "Service Time (x)", col = "lightblue", border = "white",
     ylim = c(0, 1000)) 

# Overlay the theoretical PDF (scaled to match histogram)
# f(x) = 0.5 * (1 + x) * exp(-x)
x_vals1 <- seq(0, max(samples_m1), length.out = 100)
y_vals1 <- (0.5 * (1 + x_vals1) * exp(-x_vals1)) * scaling_factor1
lines(x_vals1, y_vals1, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Simulated", "Theoretical PDF"), 
       col = c("lightblue", "red"), lwd = c(NA, 2), pch = c(15, NA))

dev.off() # Close PNG device

# Plot Histogram for Mechanic 2 (Inverse Transform Method)

# Save plot to PNG file
png("mechanic_2_histogram.png", width = 800, height = 600)

# First compute histogram without plotting to extract bin width
h2 <- hist(samples_m2, breaks = 50, plot = FALSE)
bin_width2 <- h2$breaks[2] - h2$breaks[1]

# Calculate scaling factor: N_sim * bin_width
scaling_factor2 <- N_sim * bin_width2

# Plot the histogram
hist(samples_m2, breaks = 50, main = "Mechanic 2: Inverse Transform Simulation",
     xlab = "Service Time (x)", col = "lightgreen", border = "white",
     ylim = c(0, 1000))

# Overlay the theoretical PDF (scaled to match histogram)
# f(x) = derivative of F(x) = 6x * exp(-3x^2)
x_vals2 <- seq(0, max(samples_m2), length.out = 100)
y_vals2 <- (6 * x_vals2 * exp(-3 * x_vals2^2)) * scaling_factor2
lines(x_vals2, y_vals2, col = "darkgreen", lwd = 2)

# Add legend
legend("topright", legend = c("Simulated", "Theoretical PDF"), 
       col = c("lightgreen", "darkgreen"), lwd = c(NA, 2), pch = c(15, NA))

dev.off() # Close PNG device


print("Plots saved to your folder.")
