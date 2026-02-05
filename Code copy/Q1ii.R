set.seed(1)

#========================
# Question 1 (ii)
# Inspiration taken from:
# Exercise 3 Seminar 8
#========================


#========================
k = 1 # counter for the number of iterations

K = 500 # total number of iterations

# We use Discrete Event Simulation (DES) to simulate two queues with 1 sever 
# each with an arrival process that is in part deterministic (i.e. we know in
# advance when the arrival will occur) and in part determined by an exponential
# distribution with mu = 0.25 which is the rate at which scooters breakdown.

# We simulate the first T time units of this process until Foodys becomes
# under-resourced, this happens when the parking lot is empty (i.e. our P vector
# is empty) and within the working scooters one either breaks-down 
# (with Exp(mu)) or goes for a pre-determined check-up (with c[i] = t + c). We 
# then stop because we would have no scooters to immediately replace the working
# scooter which left, thus, leading to the company being under-resourced.

# We start at T = inf given that we do not know when Foodys will become
# under-resourced (i.e. when we will stop)
T = 0

# We initialise this vector to record all instances of T
Total_T = c()

# We initialise our acceptable variance values for question 1.2 
d = 0.3 / 1.96

# We use the function from seminar 8 question 3
gen_sample_mean_var = function(k, Z, Z_bar, S_sq){
  
  Z_bar_new = Z_bar + (Z-Z_bar)/(k+1)
  
  S_sq_new = (1-(1/k))*S_sq + (k+1)*(Z_bar_new - Z_bar)^2
  
  return(c(Z_bar_new,S_sq_new))
}

# initialise the variables for the confidence interval
Z_bar = T # T_bar is the sample mean at iteration k
S_sq = 0 # S_sq is the sample variance at iteration k

# We initialise two lists one to store all state spaces for each simulation and 
# one to store all event lists for each simulation
all_ST_lists = list()
all_event_histories = list()

#========================


#========================
# We start the loop
while (k <= K) {

# lambda is not used because there is no arrival Poisson Process in this 
# exercise, instead, we have 10 individual exponential random variables with the
# same parameter mu in the following way:
# Y = -(1/mu) * log(runif(1))

# Arrival Process Parameters

# This is the average breakdown time for scooters which makes part of our
# Arrival process (in scooters per day)
mu = 0.25

# This is the mandatory check-up after consecutive days of working which
# completes our arrival process
c = 2

# Service Process Parameters

# Average service time in scooters per day for the check-up
mu_1 = 1.5

# Average service time in scooters per day for the repair
mu_2 = 2

# Total "customers" involved

n = 10 # working scooters
N = 20 # total scooters

# Initialise 

# Time
t= 0 

# State space 
Q_1 = c() # customers in the check-up queue system in time t (including 
# customers being served)
Q_2 = c() # customers in the repair queue system in time t (including customers 
# being served)
W = c(seq(from = 1, to = n)) # scooters 1 to 10 are working 
P = c(seq(from = n+1, to = N)) # scooters 11 to 20 are in the parking
# We change the usual matrix based tracking to lists so we can store the full
# information on each vector
ST_list = list()

# Event variables
c_n <- rep(Inf, N) # as per the instructions, the first n scooters are not 
# subject to a check-up until they breakdown
b_n <- rep(Inf, N)
b_n[1:n] <- t - (1/mu) * log(runif(n)) # the first 10 scooters are working, the 
# rest are not so have no breakdown time
t_1 = Inf # time of service completion for check-ups, we initialise mechanic 1
# as idle
t_2 = Inf # time of service completion for repairs, we initialise mechanic 2 
# as idle

# Again, we change the usual event_list matrix with a list allowing us to store
# vectors
event_history =  list()

# we initialise an event counter to count how many events occurred during the
# simulation
event_counter = 0

# Output Variable is already defined above as T

# Main loop
flag = 1

while (flag){
  
  # to simplify our approach we start by extracting the earliest breakdown
  # and earliest check-up
  earliest_breakdown = min(b_n)
  earliest_checkup = min(c_n)
  
  # the next event is simply the minimum between: earliest_breakdown, 
  # earliest_checkup, t_1, t_2
  
######## Case 1: Breakdown causing an arrival in Queue 2 #################################
  if (earliest_breakdown == min(earliest_breakdown, earliest_checkup, t_1, t_2)) {
    
    # We record the next time step as being the earliest break down
    t <- earliest_breakdown
    
    # We record the index of the scooter that has broken down
    i <- which.min(b_n)
    
    # We start by removing the relevant scooter from the work vector
    W <- W[!W %in% c(i)]
    
    # We then add the relevant scooter to our Q_2 (this is equivalent to counting but
    # we also keep track of which scooter through the index)
    Q_2 <- append(Q_2, i)
    
    # The scooter is no longer working
    b_n[i] <- Inf
    c_n[i] <- Inf
    
    # We check if we must stop the process
    if (length(P) == 0) {
      # No scooters left in the parking lot
      T <- t
      break
    } 
    else {
      # Appending one of the scooters left in the parking lot
      W <- append(W, P[length(P)])
      # Removing the scooter from the parking lot
      P <- P[!P %in% P[length(P)]]
      
      # Now we must assign breakdown and checkup times to our new scooter
      b_n[W[length(W)]] <- t - (1/mu) * log(runif(1))
      c_n[W[length(W)]] <- t + c
      
    }
    
    # We check if Q_2 was empty and, if so, we set a departure time
    if (length(Q_2)-1 == 0) {
      Y = -(1/mu_2) * log(runif(1)) # generate an exp(mu) RV
      t_2 = t + Y # set time of departure from s1
    }
    
    # we increment the event counter as case 1 has happened
    event_counter = event_counter + 1
    
    # we add the new values to our state space list
    ST_list[[event_counter]] = list(Q_1 = Q_1, Q_2 = Q_2, W = W, P = P, t = t)
    
    # we add the new values to our state space list
    event_history[[event_counter]] = list(c_n = c_n, b_n = b_n, t_1 = t_1, 
                                          t_2 = t_2)
    
######## Case 2: Check-up causing an arrival in Queue 1 #################################    
  } else if (earliest_checkup == min(earliest_breakdown, earliest_checkup, t_1, t_2)) {
    
    # We record the next time step as being the earliest check-up
    t <- earliest_checkup
    
    # We record the index of the scooter that has to have a check-up
    i <- which.min(c_n)
    
    # We start by removing the relevant scooter from the work vector
    W <- W[!W %in% c(i)]
    
    # We then add the relevant scooter to our Q_1 (this is equivalent to counting but
    # we also keep track of which scooter through the index)
    Q_1 <- append(Q_1, i)
    
    # The scooter is no longer working
    b_n[i] <- Inf
    c_n[i] <- Inf
    
    # We check if we must stop the process
    if (length(P) == 0) {
      # No scooters left in the parking lot
      T <- t
      break
    } 
    else {
      # Appending one of the scooters left in the parking lot
      W <- append(W, P[length(P)])
      # Removing the scooter from the parking lot
      P <- P[!P %in% P[length(P)]]
      
      # Now we must assign breakdown and checkup times to our new scooter
      b_n[W[length(W)]] <- t - (1/mu) * log(runif(1))
      c_n[W[length(W)]] <- t + c
      
    }
    
    # We check if Q_1 was empty and, if so, we set a departure time
    if (length(Q_1)-1 == 0) {
      Y = -(1/mu_1) * log(runif(1)) # generate an exp(mu) RV 
      t_1 = t + Y # set time of departure from s1
    }

    # we increment the event counter as case 2 has happened
    event_counter = event_counter + 1
    
    # we add the new values to our state space list
    ST_list[[event_counter]] = list(Q_1 = Q_1, Q_2 = Q_2, W = W, P = P, t = t)
    
    # we add the new values to our state space list
    event_history[[event_counter]] = list(c_n = c_n, b_n = b_n, t_1 = t_1, 
                                          t_2 = t_2)
    
######## Case 3: The Mechanic 1 in Queue 1 is done #################################    
  } else if (t_1 == min(earliest_breakdown, earliest_checkup, t_1, t_2)) {
    
    # We record the next time step as being the finishing time of mechanic 1
    t <- t_1
    
    # We record the index of the scooter that was having a check-up which is the
    # first scooter that was added to the queue
    i <- Q_1[1]
    
    # We remove the scooter from the queue
    Q_1 <- Q_1[!Q_1 %in% c(Q_1[1])]    
    
    # We add the scooter to the parking lot
    P <- append(P, i)
    
    # We set t_1 to being idle if the queue becomes empty
    if (length(Q_1) == 0) {
      t_1 <- Inf  
    } else {
      Y = -(1/mu_1) * log(runif(1)) # generate an exp(mu) RV 
      t_1 = t + Y # set time of departure from mechanic 1
    }
    
    # we increment the event counter as case 3 has happened
    event_counter = event_counter + 1
    
    # we add the new values to our state space list
    ST_list[[event_counter]] = list(Q_1 = Q_1, Q_2 = Q_2, W = W, P = P, t = t)
    
    # we add the new values to our state space list
    event_history[[event_counter]] = list(c_n = c_n, b_n = b_n, t_1 = t_1, 
                                          t_2 = t_2)
    
######## Case 4: The Mechanic 2 in Queue 2 is done #################################    
  } else if (t_2 == min(earliest_breakdown, earliest_checkup, t_1, t_2)) {
    
    # We record the next time step as being the finishing time of mechanic 1
    t <- t_2
    
    # We record the index of the scooter that was having a check-up which is the
    # first scooter that was added to the queue
    i <- Q_2[1]
    
    # We remove the scooter from the queue
    Q_2 <- Q_2[!Q_2 %in% c(Q_2[1])]    
    
    # We add the scooter to the parking lot
    P <- append(P, i)
    
    # We set t_2 to being idle if the queue becomes empty
    if (length(Q_2) == 0) {
      t_2 <- Inf  
    } else {
      Y = -(1/mu_2) * log(runif(1)) # generate an exp(mu) RV 
      t_2 = t + Y # set time of departure from mechanic 2
    }
    
    # we increment the event counter as case 4 has happened
    event_counter = event_counter + 1
    
    # we add the new values to our state space list
    ST_list[[event_counter]] = list(Q_1 = Q_1, Q_2 = Q_2, W = W, P = P, t = t)
    
    # we add the new values to our state space list
    event_history[[event_counter]] = list(c_n = c_n, b_n = b_n, t_1 = t_1, 
                                          t_2 = t_2)
    
  }
} # end of one iteration

# At the end of simulation we append the total time it took Foodys to become
# under-resourced to our Total_T vector
Total_T = append(Total_T, T)

# We also record each simulation's entire state space and event list for this 
# specific simulation
all_ST_lists[[k]] = ST_list
all_event_histories[[k]] = event_history

# In case k = 1 i.e. we just finished our first iteration, we initialise the
# sample mean Z_bar to be equal to T and the variance S_Sq to be zero.
# Otherwise, we directly use the gen_sample_mean_var function we defined prior
# to the loop
if (k == 1) {
  Z_bar <- T
  S_sq <- 0
} else {
  out <- gen_sample_mean_var(k-1, T, Z_bar, S_sq)
  Z_bar <- out[1]
  S_sq <- out[2]
}

# We print this statement when we reach the acceptable level of standard 
# deviation d
if (k >= 100 & sqrt(S_sq/k) < d) {
print(paste("We reach an acceptable s.d. of the estimator at k =", k, 
              "iterations"))
print(paste("Indeed, we see the Expected Time until Foodys is under-resourced is:",
            round(mean(Total_T),digits = 2), "days (with k=",k,
              "iterations)"))
print(paste("S_sq: the current sample variance is ", round(S_sq, digits = 4)))
print(paste("The s.d. of the estimator is ", round(sqrt(S_sq/k), digits = 4)))
break
}

# We increment k by 1 for the next iteration
k <- k + 1

} # end of all iterations
#========================
