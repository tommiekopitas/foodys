# ============================================================================
# MCMC for Traveling Salesman Problem - Maximizing Rewards
# Based on provided code structure
# ============================================================================

set.seed(1)

# ==========
# Question 4 (i)
# ==========

# ==========
# Step 1: Define the fixed variables and generate the reward matrix
# ==========

N = 10000    # number of steps we run the MC for
L = 1000     # last iterations used for analysis (after convergence)
r = 100       # Number of cities to visit (excluding city 0)

# Three beta values showing DIFFERENT behaviors:
# beta = 0.01: Very low - almost uniform distribution (random exploration)
# beta = 0.1:  Moderate - balanced exploration/exploitation  
# beta = 1:    High - strong preference for high rewards (exploitation)
beta_values = c(0.1, 1, 10)

# Initialize reward matrix v[i,j] of size (r+1) x (r+1)
# Row 1 = city 0, Row i+1 = city i
# Col 1 = city 0, Col j+1 = city j
v <- matrix(0, nrow = r+1, ncol = r+1)

# Generate v(0, j) for j = 1, ..., 10
# These are rewards for going FROM city 0 TO city j
for (j in 1:r) {
  v[1, j+1] <- runif(1)
}

# Generate v(i, j) for i = 1, ..., 10 and j ≠ i
# These are rewards for going FROM city i TO city j
# Generate random city positions
x_coords <- runif(r + 1, 0, 100)
y_coords <- runif(r + 1, 0, 100)

# Calculate distances and convert to rewards
for (i in 0:r) {
  for (j in 0:r) {
    if (i != j) {
      dist_ij <- sqrt((x_coords[i+1] - x_coords[j+1])^2 + 
                        (y_coords[i+1] - y_coords[j+1])^2)
      v[i+1, j+1] <- 100 - dist_ij  # Shorter distance = higher reward
    }
  }
}


# Print part of the reward matrix
cat("Reward Matrix v (first 6 rows/cols, row 1 = city 0):\n")
print(round(v[1:6, 1:6], 3))

# ==========
# Step 2: Function to calculate total reward V(x)
# ==========

# V(x) = sum of v(x_{i-1}, x_i) for i = 1 to r, where x_0 = 0
V <- function(x, v) {
  # First term: v(0, x[1]) - reward from city 0 to first city
  total <- v[1, x[1]+1]
  
  # Remaining terms: v(x[i], x[i+1]) for i = 1 to r-1
  for (i in 1:(length(x)-1)) {
    total <- total + v[x[i]+1, x[i+1]+1]
  }
  return(total)
}

# ==========
# Step 3: MCMC Simulation for each beta value
# ==========

# Storage for results from all beta values
all_results <- list()
best_solutions <- list()

# Set up plot area for histograms - save directly to PNG
png("part_i_histograms.png", width = 1000, height = 400)
par(mfrow = c(1, length(beta_values)))

for (b in 1:length(beta_values)) {
  
  beta <- beta_values[b]
  cat("\n========================================\n")
  cat("Running MCMC with beta =", beta, "\n")
  cat("========================================\n")
  
  # Initialize with arbitrary permutation (1, 2, ..., 10)
  X_current <- 1:r
  
  # Vector to store reward at each iteration
  X <- numeric(N)
  X[1] <- V(X_current, v)
  
  # Track the best solution found
  best_reward <- X[1]
  best_perm <- X_current
  
  # Run N iterations of MCMC
  for (n in 2:N) {
    
    # PROPOSAL STEP: Swap two random cities
    Y <- X_current
    
    # Select two distinct indices uniformly at random
    i <- sample(1:r, 1)
    j <- sample(1:r, 1)
    while (j == i) {
      j <- sample(1:r, 1)
    }
    
    # Perform the swap
    temp <- Y[i]
    Y[i] <- Y[j]
    Y[j] <- temp
    
    # Calculate rewards
    V_current <- V(X_current, v)
    V_prop <- V(Y, v)
    
    # ACCEPTANCE STEP using Metropolis-Hastings
    # Accept probability = min(1, exp(beta * (V_prop - V_current)))
    # Since pi(x) ∝ exp(beta * V(x)), the ratio is:
    # pi(Y)/pi(X) = exp(beta * V(Y)) / exp(beta * V(X)) = exp(beta * (V(Y) - V(X)))
    
    log_alpha <- beta * (V_prop - V_current)
    
    if (log(runif(1)) < log_alpha) {
      X_current <- Y  # Accept proposal
    }
    # Otherwise: reject and keep X_current unchanged
    
    # Store current reward
    X[n] <- V(X_current, v)
    
    # Update best solution if improved
    if (X[n] > best_reward) {
      best_reward <- X[n]
      best_perm <- X_current
    }
  }
  
  # Store results
  all_results[[b]] <- X
  best_solutions[[b]] <- list(reward = best_reward, perm = best_perm)
  
  # Plot histogram of last L iterations (after convergence)
  hist(X[(N-L+1):N], breaks = 50, col = "lightblue",
       xlab = "Reward V(x)", ylab = "Frequency",
       main = paste("Stationary Distribution (β =", beta, ")"))
  abline(v = mean(X[(N-L+1):N]), col = "red", lwd = 2)
  
  # Print summary statistics
  cat("Mean reward (last", L, "iterations):", round(mean(X[(N-L+1):N]), 4), "\n")
  cat("Std dev (last", L, "iterations):", round(sd(X[(N-L+1):N]), 4), "\n")
  cat("Best reward found:", round(best_reward, 4), "\n")
  cat("Best permutation:", best_perm, "\n")
}

# Close histogram PNG device
dev.off()

# ==========
# Step 4: Additional plots to demonstrate different behaviors
# ==========

png("part_i_analysis.png", width = 800, height = 600)
par(mfrow = c(2, 2))

# Plot 1: Trace plots showing chain behavior
plot(all_results[[1]], type = 'l', col = 'blue',
     main = "Trace Plots of Rewards Over Iterations",
     xlab = "Iteration", ylab = "Reward V(x)",
     ylim = c(min(unlist(all_results)), max(unlist(all_results))))
lines(all_results[[2]], col = 'green')
lines(all_results[[3]], col = 'red')
legend("bottomright", legend = paste("β =", beta_values),
       col = c("blue", "green", "red"), lty = 1, cex = 0.8)

# Plot 2: Running average to show convergence
running_avg <- function(x, window = 100) {
  n <- length(x)
  avg <- numeric(n)
  for (i in 1:n) {
    start <- max(1, i - window + 1)
    avg[i] <- mean(x[start:i])
  }
  return(avg)
}

plot(running_avg(all_results[[1]]), type = 'l', col = 'blue',
     main = "Running Average of Rewards",
     xlab = "Iteration", ylab = "Average Reward")
lines(running_avg(all_results[[2]]), col = 'green')
lines(running_avg(all_results[[3]]), col = 'red')
legend("bottomright", legend = paste("β =", beta_values),
       col = c("blue", "green", "red"), lty = 1, cex = 0.8)

# Plot 3: Boxplot comparison
boxplot(list(all_results[[1]][(N-L+1):N], 
             all_results[[2]][(N-L+1):N], 
             all_results[[3]][(N-L+1):N]),
        names = paste("β =", beta_values),
        main = "Boxplot of Rewards (Last 1000 iterations)",
        ylab = "Reward V(x)",
        col = c("lightblue", "lightgreen", "lightcoral"))

# Plot 4: Overlaid histograms
hist(all_results[[1]][(N-L+1):N], breaks = 30, col = rgb(0,0,1,0.3),
     main = "Overlaid Distributions",
     xlab = "Reward V(x)", xlim = c(2, 8))
hist(all_results[[2]][(N-L+1):N], breaks = 30, col = rgb(0,1,0,0.3), add = TRUE)
hist(all_results[[3]][(N-L+1):N], breaks = 30, col = rgb(1,0,0,0.3), add = TRUE)
legend("topleft", legend = paste("β =", beta_values),
       fill = c(rgb(0,0,1,0.3), rgb(0,1,0,0.3), rgb(1,0,0,0.3)), cex = 0.8)

dev.off()

# ==========
# Step 5: Summary table for Part (i)
# ==========

cat("\n============================================\n")
cat("PART (i) SUMMARY: Fixed Beta Comparison\n")
cat("============================================\n\n")

summary_table <- data.frame(
  Beta = beta_values,
  Mean_Reward = sapply(all_results, function(x) mean(x[(N-L+1):N])),
  Std_Dev = sapply(all_results, function(x) sd(x[(N-L+1):N])),
  Min_Reward = sapply(all_results, function(x) min(x[(N-L+1):N])),
  Max_Reward = sapply(all_results, function(x) max(x[(N-L+1):N])),
  Best_Found = sapply(best_solutions, function(x) x$reward)
)
print(round(summary_table, 4))

cat("\nBest permutations found:\n")
for (b in 1:length(beta_values)) {
  cat("β =", beta_values[b], ":", best_solutions[[b]]$perm, 
      "-> Reward =", round(best_solutions[[b]]$reward, 4), "\n")
}

# ============================================================================
# Question 4 (ii): SIMULATED ANNEALING - Changing beta during simulation
# ============================================================================

cat("\n============================================\n")
cat("PART (ii): SIMULATED ANNEALING\n")
cat("============================================\n")

# The key idea: Start with LOW beta (exploration) and gradually INCREASE it (exploitation)
# This allows the chain to explore widely at first, then settle into good solutions

run_simulated_annealing <- function(v, r, N, beta_start, beta_end, schedule = "linear") {
  
  X_current <- 1:r
  X <- numeric(N)
  betas_used <- numeric(N)
  X[1] <- V(X_current, v)
  
  best_reward <- X[1]
  best_perm <- X_current
  
  for (n in 2:N) {
    # Calculate beta based on cooling schedule
    progress <- n / N  # Goes from 0 to 1
    
    if (schedule == "linear") {
      beta <- beta_start + progress * (beta_end - beta_start)
    } else if (schedule == "exponential") {
      beta <- beta_start * (beta_end / beta_start)^progress
    } else if (schedule == "logarithmic") {
      beta <- beta_start + (beta_end - beta_start) * log(1 + progress * (exp(1) - 1))
    }
    
    betas_used[n] <- beta
    
    # Proposal: swap two random cities
    Y <- X_current
    i <- sample(1:r, 1)
    j <- sample(1:r, 1)
    while (j == i) j <- sample(1:r, 1)
    temp <- Y[i]; Y[i] <- Y[j]; Y[j] <- temp
    
    # Accept/reject
    V_current <- V(X_current, v)
    V_prop <- V(Y, v)
    log_alpha <- beta * (V_prop - V_current)
    
    if (log(runif(1)) < log_alpha) {
      X_current <- Y
    }
    
    X[n] <- V(X_current, v)
    
    if (X[n] > best_reward) {
      best_reward <- X[n]
      best_perm <- X_current
    }
  }
  
  return(list(rewards = X, betas = betas_used, 
              best_reward = best_reward, best_perm = best_perm))
}

# Run simulated annealing with different schedules
set.seed(42)
sa_linear <- run_simulated_annealing(v, r, N, beta_start = 0.001, beta_end = 2, schedule = "linear")

set.seed(42)
sa_exp <- run_simulated_annealing(v, r, N, beta_start = 0.001, beta_end = 2, schedule = "exponential")

set.seed(42)
sa_log <- run_simulated_annealing(v, r, N, beta_start = 0.001, beta_end = 2, schedule = "logarithmic")

# Print SA results
cat("\nSimulated Annealing Results:\n")
cat("Linear schedule:      Best =", round(sa_linear$best_reward, 4), 
    "Perm:", sa_linear$best_perm, "\n")
cat("Exponential schedule: Best =", round(sa_exp$best_reward, 4), 
    "Perm:", sa_exp$best_perm, "\n")
cat("Logarithmic schedule: Best =", round(sa_log$best_reward, 4), 
    "Perm:", sa_log$best_perm, "\n")

# Compare with fixed beta
cat("\nComparison with fixed beta:\n")
for (b in 1:length(beta_values)) {
  cat("Fixed β =", beta_values[b], ": Best =", round(best_solutions[[b]]$reward, 4), "\n")
}

# ==========
# Plots for Part (ii)
# ==========

png("part_ii_analysis.png", width = 800, height = 600)
par(mfrow = c(2, 2))

# Plot 1: Cooling schedules (how beta changes)
plot(sa_linear$betas, type = 'l', col = 'blue',
     main = "Cooling Schedules: Beta vs Iteration",
     xlab = "Iteration", ylab = "Beta")
lines(sa_exp$betas, col = 'green')
lines(sa_log$betas, col = 'red')
legend("topleft", legend = c("Linear", "Exponential", "Logarithmic"),
       col = c("blue", "green", "red"), lty = 1, cex = 0.8)

# Plot 2: SA reward traces
plot(sa_linear$rewards, type = 'l', col = 'blue',
     main = "Simulated Annealing: Reward Traces",
     xlab = "Iteration", ylab = "Reward V(x)")
lines(sa_exp$rewards, col = 'green')
lines(sa_log$rewards, col = 'red')
legend("bottomright", legend = c("Linear", "Exponential", "Logarithmic"),
       col = c("blue", "green", "red"), lty = 1, cex = 0.8)

# Plot 3: Comparison of SA vs fixed beta (running average)
plot(running_avg(sa_linear$rewards), type = 'l', col = 'purple', lwd = 2,
     main = "SA vs Fixed Beta (Running Average)",
     xlab = "Iteration", ylab = "Average Reward",
     ylim = c(3, 8))
lines(running_avg(all_results[[1]]), col = 'blue', lty = 2)
lines(running_avg(all_results[[2]]), col = 'green', lty = 2)
lines(running_avg(all_results[[3]]), col = 'red', lty = 2)
legend("bottomright", 
       legend = c("SA Linear", paste("Fixed β =", beta_values)),
       col = c("purple", "blue", "green", "red"),
       lty = c(1, 2, 2, 2), cex = 0.7)

# Plot 4: Final comparison boxplot
all_methods <- list(
  all_results[[1]][(N-L+1):N],
  all_results[[2]][(N-L+1):N],
  all_results[[3]][(N-L+1):N],
  sa_linear$rewards[(N-L+1):N],
  sa_exp$rewards[(N-L+1):N]
)
boxplot(all_methods,
        names = c(paste("β=", beta_values), "SA Lin", "SA Exp"),
        main = "All Methods Comparison (Last 1000)",
        ylab = "Reward", col = c("lightblue", "lightgreen", "lightcoral", "plum", "khaki"),
        las = 2, cex.axis = 0.8)

dev.off()

# ==========
# Multiple trials for robust comparison
# ==========

cat("\n============================================\n")
cat("Multiple Trial Comparison (20 runs each)\n")
cat("============================================\n")

n_trials <- 20
results_matrix <- matrix(0, nrow = n_trials, ncol = 6)
colnames(results_matrix) <- c("β=0.01", "β=0.1", "β=1", "SA_Lin", "SA_Exp", "SA_Log")

for (trial in 1:n_trials) {
  # Fixed beta runs
  for (b in 1:3) {
    X_current <- sample(1:r)
    best <- V(X_current, v)
    for (n in 2:N) {
      Y <- X_current
      i <- sample(1:r, 1); j <- sample(1:r, 1)
      while (j == i) j <- sample(1:r, 1)
      temp <- Y[i]; Y[i] <- Y[j]; Y[j] <- temp
      if (log(runif(1)) < beta_values[b] * (V(Y, v) - V(X_current, v))) X_current <- Y
      if (V(X_current, v) > best) best <- V(X_current, v)
    }
    results_matrix[trial, b] <- best
  }
  
  # SA runs
  results_matrix[trial, 4] <- run_simulated_annealing(v, r, N, 0.001, 2, "linear")$best_reward
  results_matrix[trial, 5] <- run_simulated_annealing(v, r, N, 0.001, 2, "exponential")$best_reward
  results_matrix[trial, 6] <- run_simulated_annealing(v, r, N, 0.001, 2, "logarithmic")$best_reward
}

# Final comparison table
final_table <- data.frame(
  Method = colnames(results_matrix),
  Mean_Best = colMeans(results_matrix),
  SD_Best = apply(results_matrix, 2, sd),
  Max_Best = apply(results_matrix, 2, max)
)
final_table[, 2:4] <- round(final_table[, 2:4], 4)

cat("\nFinal Comparison (20 trials):\n")
print(final_table)

# Final bar plot
png("final_comparison_new.png", width = 700, height = 500)
par(mfrow = c(1, 1))
barplot(final_table$Mean_Best, names.arg = final_table$Method,
        main = "Mean Best Reward Across 20 Trials",
        ylab = "Mean Best Reward",
        col = c("lightblue", "lightgreen", "lightcoral", "plum", "khaki", "orange"),
        las = 2, cex.names = 0.8)
arrows(x0 = seq(0.7, by = 1.2, length.out = 6),
       y0 = final_table$Mean_Best - final_table$SD_Best,
       y1 = final_table$Mean_Best + final_table$SD_Best,
       angle = 90, code = 3, length = 0.1)
dev.off()

cat("\n============================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================\n")
cat("Output files: part_i_histograms.png, part_i_analysis.png,\n")
cat("              part_ii_analysis.png, final_comparison.png\n")