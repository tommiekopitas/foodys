# Foodys Scooter Simulation
# Parameters
N <- 20      # Total scooters
n <- 10      # Working scooters
c <- 2       # Days between checkups
mu <- 0.25   # Breakdown rate per day
mu1 <- 1.5   # Checkup rate per day
mu2 <- 2     # Repair rate per day

# Single simulation run
simulate_T <- function() {
  # State variables
  time <- 0
  parking_lot <- N - n
  at_mechanic1 <- 0  # scooters at checkup
  at_mechanic2 <- 0  # scooters at repair
  
  # Track each working scooter
  scooters <- data.frame(
    id = 1:n,
    days_worked = 0,
    needs_checkup = FALSE,
    had_breakdown = FALSE
  )
  
  # Event list: (time, type, scooter_id)
  # type: 1=breakdown, 2=checkup_due, 3=checkup_done, 4=repair_done
  events <- data.frame(
    time = numeric(),
    type = integer(),
    scooter_id = integer()
  )
  
  # Initialize events for working scooters
  for (i in 1:n) {
    # Schedule breakdown
    breakdown_time <- time + rexp(1, mu)
    events <- rbind(events, data.frame(time = breakdown_time, type = 1, scooter_id = i))
  }
  
  # Main simulation loop
  while (TRUE) {
    # Check if under-resourced
    if (at_mechanic1 + at_mechanic2 > N - n) {
      return(time)
    }
    
    # Check if no more events
    if (nrow(events) == 0) {
      return(Inf)
    }
    
    # Get next event
    events <- events[order(events$time), ]
    next_event <- events[1, ]
    events <- events[-1, ]
    
    time <- next_event$time
    event_type <- next_event$type
    scooter_id <- next_event$scooter_id
    
    # Process event
    if (event_type == 1) {
      # Breakdown event
      at_mechanic2 <- at_mechanic2 + 1
      
      # Mark scooter as having had breakdown
      if (scooter_id <= nrow(scooters)) {
        scooters$had_breakdown[scooter_id] <- TRUE
      }
      
      # Schedule repair completion
      repair_time <- time + rexp(1, mu2)
      events <- rbind(events, data.frame(time = repair_time, type = 4, scooter_id = scooter_id))
      
      # Replace with scooter from parking lot
      if (parking_lot > 0) {
        parking_lot <- parking_lot - 1
        new_id <- N - parking_lot
        
        # Add new scooter to working set
        scooters <- rbind(scooters, data.frame(
          id = new_id,
          days_worked = 0,
          needs_checkup = FALSE,
          had_breakdown = FALSE
        ))
        
        # Schedule breakdown for new scooter
        breakdown_time <- time + rexp(1, mu)
        events <- rbind(events, data.frame(time = breakdown_time, type = 1, scooter_id = new_id))
        
        # Schedule checkup for new scooter
        checkup_time <- time + c
        events <- rbind(events, data.frame(time = checkup_time, type = 2, scooter_id = new_id))
      }
      
    } else if (event_type == 2) {
      # Checkup due event
      at_mechanic1 <- at_mechanic1 + 1
      
      # Schedule checkup completion
      checkup_time <- time + rexp(1, mu1)
      events <- rbind(events, data.frame(time = checkup_time, type = 3, scooter_id = scooter_id))
      
      # Replace with scooter from parking lot
      if (parking_lot > 0) {
        parking_lot <- parking_lot - 1
        new_id <- N - parking_lot
        
        # Add new scooter to working set
        scooters <- rbind(scooters, data.frame(
          id = new_id,
          days_worked = 0,
          needs_checkup = FALSE,
          had_breakdown = FALSE
        ))
        
        # Schedule breakdown for new scooter
        breakdown_time <- time + rexp(1, mu)
        events <- rbind(events, data.frame(time = breakdown_time, type = 1, scooter_id = new_id))
        
        # Schedule checkup for new scooter
        checkup_time <- time + c
        events <- rbind(events, data.frame(time = checkup_time, type = 2, scooter_id = new_id))
      }
      
    } else if (event_type == 3) {
      # Checkup done event
      at_mechanic1 <- at_mechanic1 - 1
      parking_lot <- parking_lot + 1
      
    } else if (event_type == 4) {
      # Repair done event
      at_mechanic2 <- at_mechanic2 - 1
      parking_lot <- parking_lot + 1
    }
  }
}

# Part (i): Run K=500 iterations
set.seed(1)
K <- 500
results <- replicate(K, simulate_T())

# Remove any infinite values
results <- results[is.finite(results)]

mean_T <- mean(results)
sd_T <- sd(results)

cat("Part (i): K = 500 iterations\n")
cat("Estimate of E[T]:", mean_T, "days\n")
cat("Standard deviation:", sd_T, "days\n")
cat("Number of valid runs:", length(results), "\n\n")

# Part (ii): Find sample size for 95% CI with margin of error 0.3
alpha <- 0.05
margin_error <- 0.3
z <- qnorm(1 - alpha/2)  # 1.96 for 95% CI

# Calculate required sample size
n_required <- ceiling((z * sd_T / margin_error)^2)

cat("Part (ii): Required iterations for 95% CI within 0.3 days\n")
cat("Required sample size:", n_required, "\n")

# Run the required number of iterations
if (n_required > K) {
  set.seed(123)
  results_full <- replicate(n_required, simulate_T())
  results_full <- results_full[is.finite(results_full)]
  
  mean_T_full <- mean(results_full)
  sd_T_full <- sd(results_full)
  se_full <- sd_T_full / sqrt(length(results_full))
  
  ci_lower <- mean_T_full - z * se_full
  ci_upper <- mean_T_full + z * se_full
  
  cat("Estimate of E[T]:", mean_T_full, "days\n")
  cat("Sample standard deviation:", sd_T_full, "days\n")
  cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")
  cat("Margin of error:", z * se_full, "days\n")
} else {
  cat("K=500 is already sufficient.\n")
  se <- sd_T / sqrt(length(results))
  ci_lower <- mean_T - z * se
  ci_upper <- mean_T + z * se
  
  cat("Estimate of E[T]:", mean_T, "days\n")
  cat("Sample standard deviation:", sd_T, "days\n")
  cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")
  cat("Margin of error:", z * se, "days\n")
}