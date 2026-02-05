# Clear your environment of variables
rm(list = ls())

# We generate the first T time units of a nonhomogeneous Poisson Process with 
# rate lambda(t). We find a lambda such that lambda(t) <= lambda for all
# 0 <= t <= T. Then we generate a homogeneous Poisson Process(lambda) and we 
# accept an event at time t with probability lambda(t)/lambda.

# For this example we use the function lambda(t) = 3 + (4/(t+1)) and we use
# lambda = 7 as this satisfies lambda(t) <= lambda for all 0 <= t <= T.

T = 10
lambda = 7

t = 0 # this is the current time
I = 0 # this is the number of events by time t

# Now we generate an exponential RV and add it to t; this becomes the time of
# the Ith event.
U = runif(1)
t = t - (1/lambda) * log(U)
#print(t)

# We assign S to be the empty vector and then we add to it the event times
S = c()

while (t <= T){
  
  L = (1/lambda) * (3 + (4/(t+1)))
  if (runif(1) <= L){
    I = I + 1 # counting this event
    S = c(S,t) # storing the time of this event
    # print(paste("accept"))
  } #else{print(paste("reject"))}
  
  # generate next exponential
  U = runif(1)
  t = t - (1/lambda) * log(U)
  #print(t)
}
# When t > T we stop.
# The Poisson process is given by I, the number of events, and by S, the event
# times.

print(I)
print(S)

# Run the code a few times to see what changes