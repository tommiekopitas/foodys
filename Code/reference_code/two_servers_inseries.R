# Clear your environment of variables
rm(list = ls())

# We use Discrete Event Simulation (DES) to simulate a queue with two severs
# in series whose arrival process is NPP with intensity lambda(t) given by 
# function lambdaf and whose service times are exponential with rate mu. 
# We simulate the first T time units of this process.
T = 4
lambda = 7
mu = 5
set.seed(1)

# We will use function time_next_arrival() so we source it.
source("C:/Users/PAPADAKK/Dropbox/MA424 - Simulation/MA424 2021-22/R Files - Exercises/Lecture 8/time_next_arrival.R")

# Initialize
t= 0 # time
N_A = 0 # number of arrivals by time t
N_D = 0 # number of departures by time t

# State variables
n1 = 0 # state: of the system in queue or being served by s1
n2 = 0 # state: of the system in queue or being served by s2
  # ST will be a matrix that stores our (n1,n2,t) state-time pairs that we record 
  # every time there is an event: arrival to servers 1,2 or departure.
ST = matrix(c(n1,n2,t) , nrow = 1, ncol = 3)

# Event List variables
  # use the nonhomogeneous Poisson to generate t_A = time of next arrival 
t_A =  time_next_arrival(t,lambda) 
t_1 = Inf # time of next departure from server s1
t_2 = Inf # time of next departure from server s2
event_list =  matrix(c(t_A, t_1, t_2), nrow=1, ncol=3)

# Initialize output variables
A1 = c() # A1(i) =  time of arrival of ith customer
A2 = c() # A2(i) =  time of arrival of ith customer to queue/service of s2
D = c() # D(i) =  time of departure of ith customer
T_p = 0 # time past T that the last customer departs

# Main loop
flag = 1

while(flag){
  
  if ((t_A == min(t_A,t_1,t_2)) & (t_A <= T)){ 
######### Case 1: arrival-within T #########################
    
    print(1)
    t = t_A # move to time t_A
    N_A = N_A + 1 # count the arrival
    n1 = n1 + 1 # count the state
    t_A = time_next_arrival(t, lambda) # generate time of next arrival
    
    if (n1 == 1){ # s1 was idle so the arrival goes to s1
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_1 = t + Y # set time of departure from s1
    }
    A1 = c(A1,t) # collect output data: A1(N_A) = t
    
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_1,t_2))
    
    
  } else if ((t_1 == min(t_A,t_1,t_2)) & (t_1 <= T)){ 
###### Case 2: departure from s1 - within T #######################
    
    print(2)
    t = t_1 # move to time t_1
    n1 = n1 - 1 # one less customer at the system of s1
    n2 = n2 + 1 # one more customer at the system of s2
    if (n1 == 0){ # no customers at s1
      t_1 = Inf 
    } else { # new customer at s1
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_1 = t + Y # set time of departure
    }
    if (n2 == 1){ # customer goes straight to s2 service
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_2 = t + Y # set time of departure
    }
    A2 = c(A2,t) # collect output data: A2(N_A-n1) = t
    
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_1,t_2))
    
  } else if ((t_2 == min(t_A,t_1,t_2)) & (t_2 <= T)){ 
####### Case 3: departure from s2 - within T ##################
    
    print(3)
    t = t_2 # move to time t_2
    N_D = N_D + 1 # one more departure
    n2 = n2 - 1 # one less customer at the system of s2
    if (n2 == 0){ # no customers at s2
      t_2 = Inf 
    } else if (n2 > 0) { # new customer at s2
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_2 = t + Y # set time of departure
    }
    
    D = c(D,t) # collect output data
    
    ST = rbind(ST, c(n1,n2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_1,t_2))  
    
  } else if ((min(t_A,t_1,t_2) > T) & (n1 + n2 > 0)){
###### Case 4: time ended, customers remain, go to next departure ####### 
    
    print(4)
    if (t_1 <= t_2){ # we copy the code from Case 2
      t = t_1 # move to time t_1
      n1 = n1 - 1 # one less customer at the system of s1
      n2 = n2 + 1 # one more customer at the system of s2
      if (n1 == 0){ # no customers at s1
        t_1 = Inf 
      } else { # new customer at s1
        Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
        t_1 = t + Y # set time of departure
      }
      if (n2 == 1){ # customer goes straight to s2 service
        Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
        t_2 = t + Y # set time of departure
      }
      A2 = c(A2,t) # collect output data
      ST = rbind(ST, c(n1,n2,t)) # update ST
      event_list = rbind(event_list, c(t_A,t_1,t_2))
      
    } else{ # we copy the code from case 3
      
      t = t_2 # move to time t_1
      N_D = N_D + 1 # one more departure
      n2 = n2 - 1 # one less customer at the system of s2
      if (n2 == 0){ # no customers at s2
        t_2 = Inf 
      } else if (n2 > 0) { # new customer at s2
        Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
        t_2 = t + Y # set time of departure
      }
      D = c(D,t) # collect output data
      ST = rbind(ST, c(n1,n2,t)) # update ST
      event_list = rbind(event_list, c(t_A,t_1,t_2)) 
      
    }
    
  } else if ((min(t_A,t_1,t_2) > T) & (n1 + n2  == 0)){ 
####### Case 5: time ended, customers gone, the end ################
    
    print(5)
    flag = 0
    T_p =  max(t-T,0)
    
  } else {
    print(paste("I should not be here"))
  }
}

print(paste("T_P = ", T_p))

print(paste("Average time customers spend in the system = ", mean(D-A1)))
print(paste("Average time customers spend queueing/served at s1 = ", mean(A2-A1)))
print(paste("Average time customers spend queueing/served at s2 = ", mean(D-A2)))
