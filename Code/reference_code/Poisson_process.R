# Clear your environment of variables
rm(list = ls())

# We generate the first T time units of a homogeneous Poisson Process (lambda).
# We do it by generating the interarrival times that are exp(lambda) and adding
# them together to get the arrival times of events, S(k) ~ gamma(k,lambda)

T = 10
lambda = 2

t = 0 # this is the current time
I = 0 # this is the number of events by time t

# Now we generate an exponential RV and add it to t; this becomes the time of
# the Ith event.
U = runif(1)
t = t - (1/lambda) * log(U)


# We assign S to be the empty vector and then we add to it the event times
S = c()

while (t <= T){
  I = I + 1 # counting this event
  S = c(S,t) # storing the time of this event
  
  # generate next exponential
  U = runif(1)
  t = t - (1/lambda) * log(U)

}
# When t > T we stop.
# The Poisson process is given by I, the number of events, and by S, the event
# times.

print(I)
print(S)

# Run the code a few times to see what changes