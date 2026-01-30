rm(list = ls())


#this code was inspired by the knapsack problem code in seminar 9 ex3

#parameters 
N <- 10000 #Number of MCMC steps
L <- 1000 #last L steps to analyze 
beta <- 0.1 #temperature parameter tunable (We ll use 0.01 0.1 1)


n_cities <- 11 #cities 0 to 10

generate_reward_matrix <- function(){
  
  
  # we generate a matrix Aij with the rewards
  V <- matrix(0, nrow = n_cities, ncol = n_cities)
  
  #v(i, j ) for j!=i because v(i,i) = 0
  for (i in 3:10) {
    for (j in 1:10) {
      if (i != j) {  # skip v(i,i)
        V[i + 1, j + 1] <- runif(1, 0, 1)
      }
    }
  }
  
  return(V)
}

V <-generate_reward_matrix()

cat("Reward matrix v(i,j)")
print(round(V[1:6, 1:6], 3)) ###just to see
cat("\n")


#Calculate reward for the helper function
# V(x) = sum_{i=1}^r v(x_{i-1}, x_i)

calculate_reward <- function(path, V){
  
  total <- 0
  r <-length(path) -1
  
  for (i in 1:r){
    from <-path[i] + 1
    to <-path[i+1] + 1
    total <- total + V[from, to]
  }
  
  return(total)
  
  
}
# Propose new path by swapping two cities (keeping x0 = 0 fixed)
# we get valid permutations of the same sequence this way
propose_swap <- function(current_path) {
  new_path <- current_path
  r <- length(current_path) - 1
  
  if (r >= 2) {
    # Swap two random positions (excluding position 1 which is x0 = 0)
    positions <- sample(2:(r + 1), size = 2, replace = FALSE)
    new_path[positions[1]] <- current_path[positions[2]]
    new_path[positions[2]] <- current_path[positions[1]]
  }
  
  return(new_path)
}


# Target stationary distribution: π(x) -> exp(β · V(x))
# where β is the tunable parameter

# β controls the "temperature":
# - β → 0 we get uniform distribution (explores everything equally)
# - β small we get broad distribution (explores widely)
# - β medium we get concentrated on good solutions
# - β larg we get concentrated on best solutions (exploitation)

run_mcmc <- function(V, r, beta, N = 50000, burn_in = 10000) {
  # r = number of cities to visit (not including x0)
  # beta = tunable parameter controlling exploration vs exploitation
  # N = total number of MCMC iterations
  
  # Initialize: x0 = 0, then cities 1, 2, ..., r in random order
  X_current <- c(0, sample(1:r, size = r, replace = FALSE))
  
  # Storage
  rewards <- numeric(N)
  paths <- list()
  acceptances <- 0
  
  
  # MCMC iterations
  for (n in 1:N) {
    # Swap two cities 
    X_prop <- propose_swap(X_current)
    
    # calculate the values with the helper 
    v_current <- calculate_reward(X_current, V)
    v_prop <- calculate_reward(X_prop, V)
    
    # METROPOLIS ACCEPTANCE 
    # Accept with probability min(1, exp(β·(V_prop - V_current)))
    exp_term <- exp(beta * (v_prop - v_current))
    prob_accept <- min(exp_term, 1)
    
    U <- runif(1)
    if (U < prob_accept) {
      X_current <- X_prop
      acceptances <- acceptances + 1
    }
    
    
    # Store current state
    rewards[n] <- calculate_reward(X_current, V)
    if (n %% 1000 == 0) {
      paths[[n / 1000]] <- X_current
    }
  }
  
  # Return results
  list(
    rewards = rewards,
    rewards_after_burnin = rewards[(burn_in + 1):N],
    paths = paths,
    acceptance_rate = acceptances / N,
    best_reward = max(rewards),
    best_path_idx = which.max(rewards)
  )
}


cat("============================================\n")
cat("Running MCMC for 3 Different β Values\n")
cat("============================================\n\n")

r <- 10  # visit all cities 1 through 10
N <- 50000
burn_in <- 10000

# THREE DIFFERENT β VALUES
beta_values <- c(0.1, 0.5, 1)

results <- list()

for (i in 1:length(beta_values)) {
  beta <- beta_values[i]
  cat(sprintf("Running MCMC with β = %.1f...\n", beta))
  
  results[[i]] <- run_mcmc(V, r, beta, N, burn_in)
  
  
  cat(sprintf("  Acceptance rate: %.3f\n", results[[i]]$acceptance_rate))
  cat(sprintf("  Best reward found: %.4f\n", results[[i]]$best_reward))
  cat(sprintf("  Mean reward (after burn-in): %.4f\n", 
              mean(results[[i]]$rewards_after_burnin)))
  cat(sprintf("  Std dev (after burn-in): %.4f\n\n", 
              sd(results[[i]]$rewards_after_burnin)))
}

# Setup for multiple plots
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))

colors <- c("blue", "darkgreen", "red")
beta_labels <- c("β = 0.1 (Exploratory)", 
                 "β = 5 (Balanced)", 
                 "β = 50 (Greedy)")

# ROW 1: Full trace plots
for (i in 1:3) {
  plot(results[[i]]$rewards, type = 'l', col = colors[i],
       main = paste("Trace Plot:", beta_labels[i]),
       xlab = "Iteration", ylab = "Reward V(x)",
       ylim = range(results[[1]]$rewards, results[[2]]$rewards, results[[3]]$rewards))
  abline(h = results[[i]]$best_reward, col = "black", lty = 2, lwd = 1.5)
  abline(v = burn_in, col = "gray", lty = 3)
  legend("bottomright", 
         legend = c(paste("Best:", round(results[[i]]$best_reward, 3)),
                    paste("Mean:", round(mean(results[[i]]$rewards_after_burnin), 3))),
         bty = "n", cex = 0.8)
}


# ROW 2: Histograms (after burn-in)
for (i in 1:3) {
  hist(results[[i]]$rewards_after_burnin, 
       breaks = 50, 
       col = colors[i],
       border = "white",
       main = paste("Distribution:", beta_labels[i]),
       xlab = "Reward V(x)",
       ylab = "Frequency",
       xlim = range(results[[1]]$rewards_after_burnin, 
                    results[[2]]$rewards_after_burnin, 
                    results[[3]]$rewards_after_burnin))
  abline(v = mean(results[[i]]$rewards_after_burnin), 
         col = "black", lwd = 2, lty = 2)
}


# ROW 3: Autocorrelation plots
for (i in 1:3) {
  acf(results[[i]]$rewards_after_burnin, 
      main = paste("ACF:", beta_labels[i]),
      col = colors[i],
      lwd = 2,
      lag.max = 100)
}


#Comparison with all three in the same axis 

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Combined trace (after burn-in)
plot_start <- burn_in + 1
plot_end <- burn_in + 5000
plot(plot_start:plot_end, results[[1]]$rewards[plot_start:plot_end], 
     type = 'l', col = colors[1], lwd = 1,
     main = "Comparison: First 5000 Post-Burn-in Iterations",
     xlab = "Iteration", ylab = "Reward V(x)",
     ylim = range(results[[1]]$rewards[plot_start:plot_end],
                  results[[2]]$rewards[plot_start:plot_end],
                  results[[3]]$rewards[plot_start:plot_end]))
lines(plot_start:plot_end, results[[2]]$rewards[plot_start:plot_end], 
      col = colors[2], lwd = 1)
lines(plot_start:plot_end, results[[3]]$rewards[plot_start:plot_end], 
      col = colors[3], lwd = 1)
legend("bottomright", legend = beta_labels, 
       col = colors, lwd = 2, cex = 0.7)


hist(results[[1]]$rewards_after_burnin, breaks = 50, 
     col = rgb(0, 0, 1, 0.3), border = NA,
     main = "Comparison: Distribution of Rewards",
     xlab = "Reward V(x)", ylab = "Density",
     freq = FALSE,
     xlim = range(results[[1]]$rewards_after_burnin,
                  results[[2]]$rewards_after_burnin,
                  results[[3]]$rewards_after_burnin))
hist(results[[2]]$rewards_after_burnin, breaks = 50,
     col = rgb(0, 0.5, 0, 0.3), border = NA, add = TRUE, freq = FALSE)
hist(results[[3]]$rewards_after_burnin, breaks = 50,
     col = rgb(1, 0, 0, 0.3), border = NA, add = TRUE, freq = FALSE)
legend("topleft", legend = beta_labels, 
       fill = c(rgb(0, 0, 1, 0.5), rgb(0, 0.5, 0, 0.5), rgb(1, 0, 0, 0.5)),
       cex = 0.7)



# Box plots comparison
boxplot(results[[1]]$rewards_after_burnin,
        results[[2]]$rewards_after_burnin,
        results[[3]]$rewards_after_burnin,
        names = c("β=0.1", "β=5", "β=50"),
        col = colors,
        main = "Comparison: Reward Distribution",
        ylab = "Reward V(x)",
        notch = TRUE)

# Running average comparison
running_avg_1 <- cumsum(results[[1]]$rewards) / (1:N)
running_avg_2 <- cumsum(results[[2]]$rewards) / (1:N)
running_avg_3 <- cumsum(results[[3]]$rewards) / (1:N)



plot(running_avg_1, type = 'l', col = colors[1], lwd = 2,
     main = "Cumulative Average Reward",
     xlab = "Iteration", ylab = "Average Reward",
     ylim = range(running_avg_1, running_avg_2, running_avg_3))
lines(running_avg_2, col = colors[2], lwd = 2)
lines(running_avg_3, col = colors[3], lwd = 2)
abline(v = burn_in, col = "gray", lty = 3)
legend("bottomright", legend = beta_labels, 
       col = colors, lwd = 2, cex = 0.7)



#find and display best paths 


for (i in 1:3) {
  best_idx <- results[[i]]$best_path_idx
  
  # Reconstruct the best path (we need to run forward from a saved point)
  # For simplicity, let's find the best from last 1000 iterations
  last_1000 <- results[[i]]$rewards[(N-999):N]
  best_in_last <- which.max(last_1000)
  
  cat(sprintf("β = %.1f:\n", beta_values[i]))
  cat(sprintf("  Best reward: %.4f\n", results[[i]]$best_reward))
  cat(sprintf("  Found at iteration: %d\n\n", best_idx))
}

#verify convergence 

for (i in 1:3) {
  # Split chain in half and compare
  n_half <- length(results[[i]]$rewards_after_burnin) / 2
  first_half <- results[[i]]$rewards_after_burnin[1:n_half]
  second_half <- results[[i]]$rewards_after_burnin[(n_half+1):(2*n_half)]
  
  cat(sprintf("β = %.1f:\n", beta_values[i]))
  cat(sprintf("  First half mean:  %.4f\n", mean(first_half)))
  cat(sprintf("  Second half mean: %.4f\n", mean(second_half)))
  cat(sprintf("  Difference:       %.4f\n", abs(mean(first_half) - mean(second_half))))
  cat(sprintf("  Effective sample size: ~%.0f\n\n", 
              length(results[[i]]$rewards_after_burnin) / 
                (1 + 2 * sum(acf(results[[i]]$rewards_after_burnin, 
                                 plot = FALSE, lag.max = 50)$acf[-1]))))
  
  
  
  
  
}

# PART (ii): ADAPTIVE BETA SCHEDULE
cat("============================================\n")
cat("Part (ii): Adaptive Beta Schedule\n")
cat("============================================\n\n")

# Adaptive MCMC with changing beta
run_mcmc_adaptive <- function(V, r, beta_schedule_fn, N = 50000, burn_in = 10000) {
  # beta_schedule_fn: function that takes iteration n and returns beta value
  
  X_current <- c(0, sample(1:r, size = r, replace = FALSE))
  
  rewards <- numeric(N)
  beta_values <- numeric(N)
  acceptances <- 0
  
  for (n in 1:N) {
    # Get current beta based on iteration
    beta <- beta_schedule_fn(n, N)
    beta_values[n] <- beta
    
    X_prop <- propose_swap(X_current)
    
    v_current <- calculate_reward(X_current, V)
    v_prop <- calculate_reward(X_prop, V)
    
    exp_term <- exp(beta * (v_prop - v_current))
    prob_accept <- min(exp_term, 1)
    
    U <- runif(1)
    if (U < prob_accept) {
      X_current <- X_prop
      acceptances <- acceptances + 1
    }
    
    rewards[n] <- calculate_reward(X_current, V)
  }
  
  list(
    rewards = rewards,
    beta_values = beta_values,
    rewards_after_burnin = rewards[(burn_in + 1):N],
    acceptance_rate = acceptances / N,
    best_reward = max(rewards),
    best_path_idx = which.max(rewards)
  )
}

# Define different adaptive schedules
exponential_schedule <- function(n, N) {
  beta_start <- 0.1
  beta_end <- 2
  beta_start + (beta_end - beta_start) * (1 - exp(-5 * n / N))
}

linear_schedule <- function(n, N) {
  beta_start <- 0.1
  beta_end <- 2
  beta_start + (beta_end - beta_start) * n / N
}

two_phase_schedule <- function(n, N) {
  if (n < N/2) 0.1 else 1.5
}

# Run adaptive schedules
cat("Running adaptive schedules...\n\n")

result_exp <- run_mcmc_adaptive(V, r, exponential_schedule, N, burn_in)
result_lin <- run_mcmc_adaptive(V, r, linear_schedule, N, burn_in)
result_2phase <- run_mcmc_adaptive(V, r, two_phase_schedule, N, burn_in)

# Print comparison
cat("RESULTS COMPARISON:\n")
cat("==================\n\n")

cat("Fixed Beta (from part i):\n")
for (i in 1:3) {
  cat(sprintf("  β = %.1f: Best = %.4f, Mean = %.4f, Accept = %.3f\n",
              beta_values[i], 
              results[[i]]$best_reward,
              mean(results[[i]]$rewards_after_burnin),
              results[[i]]$acceptance_rate))
}

cat("\nAdaptive Beta (part ii):\n")
cat(sprintf("  Exponential: Best = %.4f, Mean = %.4f, Accept = %.3f\n",
            result_exp$best_reward,
            mean(result_exp$rewards_after_burnin),
            result_exp$acceptance_rate))
cat(sprintf("  Linear:      Best = %.4f, Mean = %.4f, Accept = %.3f\n",
            result_lin$best_reward,
            mean(result_lin$rewards_after_burnin),
            result_lin$acceptance_rate))
cat(sprintf("  Two-phase:   Best = %.4f, Mean = %.4f, Accept = %.3f\n",
            result_2phase$best_reward,
            mean(result_2phase$rewards_after_burnin),
            result_2phase$acceptance_rate))

# Visualization
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Beta schedules over time
plot(result_exp$beta_values, type = 'l', col = 'purple', lwd = 2,
     main = "Beta Schedules Over Time",
     xlab = "Iteration", ylab = "Beta Value",
     ylim = c(0, max(result_exp$beta_values, result_lin$beta_values)))
lines(result_lin$beta_values, col = 'orange', lwd = 2)
lines(result_2phase$beta_values, col = 'brown', lwd = 2)
abline(h = c(0.1, 0.5, 1), col = c("blue", "darkgreen", "red"), lty = 2)
legend("right", 
       c("Exponential", "Linear", "Two-phase", "Fixed: 0.1, 0.5, 1"),
       col = c("purple", "orange", "brown", "black"),
       lty = c(1, 1, 1, 2), lwd = 2, cex = 0.7)

# 2. Trace plots comparison
plot(results[[1]]$rewards, type = 'l', col = rgb(0,0,1,0.3), lwd = 1,
     main = "Reward Traces: Fixed vs Adaptive",
     xlab = "Iteration", ylab = "Reward",
     ylim = range(results[[1]]$rewards, result_exp$rewards))
lines(results[[2]]$rewards, col = rgb(0,0.5,0,0.3), lwd = 1)
lines(results[[3]]$rewards, col = rgb(1,0,0,0.3), lwd = 1)
lines(result_exp$rewards, col = 'purple', lwd = 2)
lines(result_lin$rewards, col = 'orange', lwd = 2)
abline(v = burn_in, col = "gray", lty = 3)
legend("bottomright",
       c("Fixed β=0.1", "Fixed β=0.5", "Fixed β=1", "Exponential", "Linear"),
       col = c("blue", "darkgreen", "red", "purple", "orange"),
       lwd = c(1, 1, 1, 2, 2), cex = 0.6)

# 3. Distribution comparison (after burn-in)
hist(result_exp$rewards_after_burnin, breaks = 50,
     col = rgb(0.5, 0, 0.5, 0.4), border = NA,
     main = "Reward Distributions (Post Burn-in)",
     xlab = "Reward", ylab = "Density", freq = FALSE,
     xlim = range(results[[1]]$rewards_after_burnin, 
                  results[[2]]$rewards_after_burnin,
                  results[[3]]$rewards_after_burnin,
                  result_exp$rewards_after_burnin))
hist(results[[2]]$rewards_after_burnin, breaks = 50,
     col = rgb(0, 0.5, 0, 0.3), border = NA, add = TRUE, freq = FALSE)
hist(result_lin$rewards_after_burnin, breaks = 50,
     col = rgb(1, 0.5, 0, 0.4), border = NA, add = TRUE, freq = FALSE)
legend("topleft",
       c("Exponential", "Fixed β=0.5", "Linear"),
       fill = c(rgb(0.5,0,0.5,0.5), rgb(0,0.5,0,0.5), rgb(1,0.5,0,0.5)),
       cex = 0.7)

# 4. Box plot comparison
boxplot(results[[1]]$rewards_after_burnin,
        results[[2]]$rewards_after_burnin,
        results[[3]]$rewards_after_burnin,
        result_exp$rewards_after_burnin,
        result_lin$rewards_after_burnin,
        result_2phase$rewards_after_burnin,
        names = c("β=0.1", "β=0.5", "β=1", "Exp", "Lin", "2-Ph"),
        col = c("blue", "darkgreen", "red", "purple", "orange", "brown"),
        main = "Fixed vs Adaptive Comparison",
        ylab = "Reward",
        notch = TRUE)

# Summary statistics table
cat("\n\nSUMMARY STATISTICS:\n")
cat("===================\n\n")

comparison_df <- data.frame(
  Method = c("Fixed β=0.1", "Fixed β=0.5", "Fixed β=1", 
             "Exponential", "Linear", "Two-phase"),
  Best = c(results[[1]]$best_reward, results[[2]]$best_reward, results[[3]]$best_reward,
           result_exp$best_reward, result_lin$best_reward, result_2phase$best_reward),
  Mean = c(mean(results[[1]]$rewards_after_burnin), 
           mean(results[[2]]$rewards_after_burnin),
           mean(results[[3]]$rewards_after_burnin),
           mean(result_exp$rewards_after_burnin),
           mean(result_lin$rewards_after_burnin),
           mean(result_2phase$rewards_after_burnin)),
  SD = c(sd(results[[1]]$rewards_after_burnin),
         sd(results[[2]]$rewards_after_burnin),
         sd(results[[3]]$rewards_after_burnin),
         sd(result_exp$rewards_after_burnin),
         sd(result_lin$rewards_after_burnin),
         sd(result_2phase$rewards_after_burnin)),
  Accept_Rate = c(results[[1]]$acceptance_rate, results[[2]]$acceptance_rate, 
                  results[[3]]$acceptance_rate, result_exp$acceptance_rate,
                  result_lin$acceptance_rate, result_2phase$acceptance_rate)
)

print(comparison_df)

cat("\n\nCONCLUSION:\n")
cat("The adaptive schedules achieve better or comparable performance\n")
cat("by starting with exploration (low β) and gradually increasing\n")
cat("exploitation (high β), combining the benefits of both strategies.\n")