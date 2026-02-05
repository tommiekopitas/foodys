set.seed(1)

# ==========
# Question 4 (i) - WITH EUCLIDEAN REWARD MATRIX
# ==========

N = 10000 # number of steps we run the MC for
L = 1000 # last iterations plotted
r = 10 # Number of cities
beta_values = c(0.01, 0.1, 1, 5, 10, 50) # chosen Betas

# initialising a matrix of zeros of size r+1 x r+1
v <- matrix(0, nrow = r+1, ncol = r+1)

# ==========
# NEW: EUCLIDEAN REWARD MATRIX
# Cities have (x,y) positions, reward = max_dist - distance
# This creates structured correlations (triangle inequality)
# ==========

# Generate random city positions in 2D
x_coords <- runif(r + 1, 0, 100)
y_coords <- runif(r + 1, 0, 100)

cat("City positions:\n")
for (i in 0:r) {
  cat("City", i, ": (", round(x_coords[i+1], 2), ",", round(y_coords[i+1], 2), ")\n")
}

# Calculate distance matrix
d <- matrix(0, nrow = r + 1, ncol = r + 1)
for (i in 1:(r + 1)) {
  for (j in 1:(r + 1)) {
    d[i, j] <- sqrt((x_coords[i] - x_coords[j])^2 + (y_coords[i] - y_coords[j])^2)
  }
}

# Convert distance to reward: reward = max_distance - distance
# This way: shorter distance = higher reward (we want to maximize reward)
max_dist <- max(d)
v <- max_dist - d
diag(v) <- 0  # No self-loops

cat("\nReward Matrix (first 6 rows/cols):\n")
print(round(v[1:6, 1:6], 2))

cat("\nKey difference from Uniform(0,1):\n")
cat("- Rewards are CORRELATED (triangle inequality)\n")
cat("- Creates structured local optima\n")
cat("- High beta should now get TRAPPED in bad local optima\n\n")

# ==========
# Value function V(x) - same as before
# ==========

V <- function(x, v) {
  total <- v[1, x[1] + 1]
  for (i in 1:(length(x) - 1)) {
    total <- total + v[x[i] + 1, x[i + 1] + 1]
  }
  return(total)
}

# ==========
# Run MCMC for different beta values
# ==========

par(mfrow = c(2, 3))

all_results <- list()
best_solutions <- list()

for (b in 1:length(beta_values)) {
  
  beta <- beta_values[b]
  
  # Start with permutation 1, 2, ..., r
  X_current <- 1:r
  
  # Store values
  X <- numeric(N)
  X[1] <- V(X_current, v)
  
  # Track best
  best_reward <- X[1]
  best_perm <- X_current
  
  # MCMC loop
  for (n in 2:N) {
    
    # Proposal: swap two random positions
    Y <- X_current
    i <- sample(1:r, 1)
    j <- sample(1:r, 1)
    while (j == i) {
      j <- sample(1:r, 1)
    }
    
    # Swap
    temp <- Y[i]
    Y[i] <- Y[j]
    Y[j] <- temp
    
    # Calculate values
    V_current <- V(X_current, v)
    V_prop <- V(Y, v)
    
    # Accept/reject
    log_alpha <- beta * (V_prop - V_current)
    
    if (log(runif(1)) < log_alpha) {
      X_current <- Y
    }
    
    X[n] <- V(X_current, v)
    
    # Track best
    if (X[n] > best_reward) {
      best_reward <- X[n]
      best_perm <- X_current
    }
  }
  
  all_results[[b]] <- X
  best_solutions[[b]] <- list(reward = best_reward, perm = best_perm)
  
  # Plot histogram
  hist(X[(N - L + 1):N], breaks = 50, col = "lightblue",
       xlab = "Reward", main = paste("β =", beta),
       xlim = c(min(unlist(all_results)), max(unlist(all_results)) + 50))
  abline(v = mean(X[(N - L + 1):N]), col = "red", lwd = 2)
  
  cat("Beta =", beta, "\n")
  cat("  Mean (last", L, "):", round(mean(X[(N - L + 1):N]), 2), "\n")
  cat("  Std Dev:", round(sd(X[(N - L + 1):N]), 2), "\n")
  cat("  Best found:", round(best_reward, 2), "\n\n")
}

# ==========
# Summary comparison
# ==========

cat("\n========================================\n")
cat("SUMMARY WITH EUCLIDEAN REWARDS\n")
cat("========================================\n\n")

summary_df <- data.frame(
  Beta = beta_values,
  Mean = sapply(all_results, function(x) mean(x[(N - L + 1):N])),
  StdDev = sapply(all_results, function(x) sd(x[(N - L + 1):N])),
  Best = sapply(best_solutions, function(x) x$reward)
)
summary_df[, 2:4] <- round(summary_df[, 2:4], 2)
print(summary_df)

# ==========
# Plot: Average vs Beta (like your original plot)
# ==========

par(mfrow = c(1, 1))

# Test many beta values
beta_test <- seq(0.1, 20, by = 0.5)
avg_rewards <- numeric(length(beta_test))

for (b in 1:length(beta_test)) {
  beta <- beta_test[b]
  X_current <- 1:r
  X <- numeric(N)
  X[1] <- V(X_current, v)
  
  for (n in 2:N) {
    Y <- X_current
    i <- sample(1:r, 1)
    j <- sample(1:r, 1)
    while (j == i) j <- sample(1:r, 1)
    temp <- Y[i]; Y[i] <- Y[j]; Y[j] <- temp
    
    if (log(runif(1)) < beta * (V(Y, v) - V(X_current, v))) {
      X_current <- Y
    }
    X[n] <- V(X_current, v)
  }
  
  avg_rewards[b] <- mean(X[(N - L + 1):N])
}

plot(beta_test, avg_rewards, type = "l", lwd = 2,
     main = "Average Reward vs Beta (EUCLIDEAN)",
     xlab = "Beta", ylab = "Average Reward (last 1000 iterations)")
grid()

cat("\nWith Euclidean rewards, you should see:\n")
cat("- Average increases then PLATEAUS or DECREASES\n")
cat("- High beta gets trapped in LOCAL OPTIMA that are NOT optimal\n")
cat("- This is the expected MCMC behavior!\n")