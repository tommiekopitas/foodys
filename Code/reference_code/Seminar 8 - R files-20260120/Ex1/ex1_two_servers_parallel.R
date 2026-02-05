# Clear your environment of variables
rm(list = ls())

# We use Discrete Event Simulation (DES) to simulate a queue with two severs
# in parallel whose arrival process is Poisson lambda(t) given by lambdaf and 
# whose service times are exponential with rate mu. If both servers are idle 
# then first arrival goes to s1.
# We simulate the first T time units of this process.
T = 4
lambda = 7
mu = 5
set.seed(1)

# We will use function time_next_arrival() so we source it.
source("C:/Users/PAPADAKK/Dropbox/MA424 - New/Lecture 8/L8 seminar/R code/time_next_arrival.R")

# Initialize
t= 0 # time
N_A = 0 # number of arrivals by time t
C1 = 0 # number of customers served by s1 by time t
C2 = 0 # number of customers served by s1 by time t. Note that N_D = C1+C2

# State variables
n = 0 # state variable: number of customers in the system at time t
i1 = 0 # state variable: customer number at s1
i2 = 0 # state variable: customer number at s2
  # ST will be a matrix that stores our (n,i1,i2,t) state-time pairs that we record 
  # every time there is an event: arrival or departure.
ST = matrix(c(n,i1,i2,t) , nrow = 1, ncol = 4)

# Event List variables
  # use the nonhomogeneous Poisson to generate t_A = time of next arrival 
t_A =  time_next_arrival(t,lambda) 
t_1 = Inf # time of next departure from server s1
t_2 = Inf # time of next departure from server s2
event_list =  matrix(c(t_A, t_1, t_2), nrow=1, ncol=3)

# Initialize output variables
A = c() # A(i) =  time of arrival of ith customer
  # For Departure we use a different way of storing values: Dept stores (i,D(i)) 
  # pairs - we sort these with respect to i at the end
Dept =  matrix(0, nrow=0, ncol=2) 
T_p = 0 # time past T that the last customer departs

# Main loop
flag = 1

while(flag){
  
  if ((t_A == min(t_A,t_1,t_2)) & (t_A <= T)){ 
######## Case 1: arrival-within T #################################
    
    print(1)
    t = t_A # move to time t_A
    N_A = N_A + 1 # count the arrival
    t_A = time_next_arrival(t, lambda) # generate time of next arrival
    
    if (n == 0){ # system empty so the arrival goes to s1
      n = n+1
      i1 = N_A
      i2 = 0
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_1 = t + Y # set time of departure from s1
    
    } else if ((n == 1) & (i2 == 0)){ # s1 busy but s2 idle, arrival to s2
      n = n+1
      i2 = N_A
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_2 = t + Y # set time of departure from s1
      
    } else if ((n==1) & (i1==0)){ # s2 busy but s1 idle, arrival to s1
      n = n+1
      i1 = N_A
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_1 = t + Y # set time of departure from s1
      
    } else if (n > 1){# both servers busy, new customer queues
      n = n+1
      
    } else {print(paste("I should not be here"))}
    
    A = c(A,t) # collect output data
      # Note: arrival of customers happens in the same order as customer number.
      # This is not the same as departures.
    ST = rbind(ST, c(n,i1,i2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_1,t_2))
    
    
  } else if ((t_1 == min(t_A,t_1,t_2)) & (t_1 <= T)){ 
######### Case 2: departure from s1 - within T ####################
    
    print(2)
    t = t_1 # move to time t_1
    C1 = C1 + 1 # one more departure from s1.
    Dept = rbind(Dept, c(i1,t))
    
    if ((n == 1) | (n == 2)){ # no customer at queue so s1 remains idle
      n = n - 1
      i1 = 0
      t_1 = Inf 
    } else if (n > 2){ # next customer at queue goes to s1
      m = max(i1,i2) # we calculate m because m+1 is the next customer
      n = n - 1
      i1 = m+1
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_1 = t + Y # set time of departure
    } else {print(paste("I should not be here"))}
  
    ST = rbind(ST, c(n,i1,i2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_1,t_2))
    
  } else if ((t_2 == min(t_A,t_1,t_2)) & (t_2 <= T)){ 
############# Case 3: departure from s2 - within T ####################
    
    print(3)
    t = t_2 # move to time t_2
    C2 = C2 + 1
    Dept = rbind(Dept, c(i2,t))
    
    if ((n == 1) | (n==2)){ # no customer at queue so s2 remains idle
      n = n - 1
      i2 = 0
      t_2 = Inf 
    } else if (n > 2) { # next customer at queue goes to s2
      m = max(i1,i2) # we calculate m because m+1 is the next customer
      n = n - 1
      i2 = m+1
      Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
      t_2 = t + Y # set time of departure
    } else {print(paste("I should not be here"))}
    
    ST = rbind(ST, c(n,i1,i2,t)) # update ST
    event_list = rbind(event_list, c(t_A,t_1,t_2))  
    
  } else if ((min(t_A,t_1,t_2) > T) & (n > 0)){
######## Case 4: time ended, customers remain, go to next departure #########
    
    print(4)
    if (t_1 <= t_2){ # we copy the code from Case 2
      
      t = t_1 # move to time t_1
      C1 = C1 + 1 # one more departure from s1.
      Dept = rbind(Dept, c(i1,t))
      
      if ((n == 1) | (n == 2)){ # no customer at queue so s1 remains idle
        n = n - 1
        i1 = 0
        t_1 = Inf 
      } else if (n > 2){ # next customer at queue goes to s1
        m = max(i1,i2) # we calculate m because m+1 is the next customer
        n = n - 1
        i1 = m+1
        Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
        t_1 = t + Y # set time of departure
      } else {print(paste("I should not be here"))}
      
      ST = rbind(ST, c(n,i1,i2,t)) # update ST
      event_list = rbind(event_list, c(t_A,t_1,t_2))
      
    } else{ # we copy the code from case 3
      
      t = t_2 # move to time t_2
      C2 = C2 + 1
      Dept = rbind(Dept, c(i2,t))
      
      if ((n == 1) | (n==2)){ # no customer at queue so s2 remains idle
        n = n - 1
        i2 = 0
        t_2 = Inf 
      } else if (n > 2) { # next customer at queue goes to s2
        m = max(i1,i2) # we calculate m because m+1 is the next customer
        n = n - 1
        i2 = m+1
        Y = -(1/mu) * log(runif(1)) # generate an exp(mu) RV
        t_2 = t + Y # set time of departure
      } else {print(paste("I should not be here"))}
      
      ST = rbind(ST, c(n,i1,i2,t)) # update ST
      event_list = rbind(event_list, c(t_A,t_1,t_2))  
      
      
    }
    
  } else if ((min(t_A,t_1,t_2) > T) & (n == 0)){ 
######## Case 5: time ended, customers gone, the end #################
    
    print(5)
    flag = 0
    T_p =  max(t-T,0)
    
  } else {
    print(paste("I should not be here"))
  }
}

print(paste("T_P = ", T_p))

# Sort customer departure times in Dept
Dept_sort = Dept[order(Dept[,1]),]

print(paste("Average time customers spend in the system = ", mean(Dept_sort[,2]-A)))

print(paste("Number of customers served by s1 = ", C1))
print(paste("Number of customers served by s2 = ", C2))