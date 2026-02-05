# Clear your environment of variables
rm(list = ls())

# We use Discrete Event Simulation (DES) to simulate a single server queue 
# whose arrival process is Nonhomogeneous Poisson with intensity lambda(t) 
# simulated by function by lambdaf, and whose service times are exponential 
# with rate mu. We simulate the first T time units of this process.

T = 10
lambda = 7
mu = 8
# set.seed(1)

# We will use function time_next_arrival() so we source it.
source("C:/Users/PAPADAKK/Dropbox/MA424 - Simulation/MA424 2021-22/R Files - Exercises/Lecture 7/time_next_arrival.R")

# Initialize
t= 0 # time
N_A = 0 # number of arrivals by time t
N_D = 0 # number of departures by time t

# State variables
n = 0 # state of the system (number of customers in system) at time t
  # ST will be a matrix that stores our (n,t) state-time pairs that we record 
  # every time there is an event: arrival or departure.
ST = matrix(c(n,t) , nrow = 1, ncol = 2)

# Event List variables
   # use the nonhomogeneous Poisson to generate t_A = time of next arrival 
t_A =  time_next_arrival(t,lambda) 
t_D = Inf # time of next departure ; set to Inf since server is idle
          # Note: first letter of Inf is Upper case.
event_list =  matrix(c(t_A, t_D), nrow=1, ncol=2)

# Initialize output variables
A = c() # A(i) =  time of arrival of ith customer
D = c() # D(i) =  time of departure of ith customer
T_p = 0 # time past T that the last customer departs

# Main loop
flag = 1

while(flag){
  
  if ((t_A <= t_D) & (t_A <= T)){ # Case 1:  arrival and within T
    
    t = t_A # move to time t_A
    N_A = N_A + 1 # count the arrival
    n = n + 1 # count the state
    t_A = time_next_arrival(t, lambda) # generate time of next arrival
    
    if (n == 1){ # system had been empty so the arrival goes to the server
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D = t + Y # set time of next departure
    }
    A = c(A,t) # collect output data: A(N_A) = t
    ST = rbind(ST, c(n,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_D))
    
    
  } else if ((t_D < t_A) & (t_D <= T)){ # Case 2: departure and within T
  
    t = t_D # move to time t_D
    N_D = N_D + 1 # count the departure
    n = n -1 # one less customer in the system
    if (n == 0){ # no customers at the server
      t_D = Inf 
    } else { # new customer at the server
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D = t + Y # set time of next departure
    }
    D = c(D,t) # collect output data: D(N_D) = t
    ST = rbind(ST, c(n,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_D))
    
  } else if ((min(t_A,t_D) > T) & (n > 0)){
    # Case 3: time ended, customers remain, go to next departure    
    
    t = t_D # ignore arrival t_A since > T but still need to deal with 
            # customers in the system so we go to the next departure
    N_D = N_D + 1 # count departure
    n = n - 1 # update state
    if (n > 0){ 
      # if we still have customers, they go to the server, 
      # if no customers then it is taken care of in the next iter under Case 4 
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_D = t + Y # set time of next departure
    }
    D = c(D,t) # collect output data: D(N_D) = t
    ST = rbind(ST, c(n,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_D))
    
    
  } else if ((min(t_A,t_D) > T) & (n  == 0)){ 
    # Case 4: time ended, customers gone, the end
    
    flag = 0
    T_p =  max(t-T,0)
    
  } else {
    print(paste("I should not be here"))
  }
}

print(paste("Average time customers spend in the system = ", mean(D-A)))
print(paste("T_P = ", T_p))

