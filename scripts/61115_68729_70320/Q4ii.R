set.seed(1)

# ==========
# Question 4 (ii)
# ==========
# Inspiration taken from:
# Exercise 3 Seminar 9
# Examples 3 and 4 from lecture 9
# ==========


# ==========
# Step 1: Define the fixed variables and
# Generate the reward matrix (i.e. generate
# the necessary data for the MC)

N = 10000 # number of steps we run the MC for
L = 1000 # last iterations plotted
r = 10 # Number of cities
beta_initial_exp = 1 # we chose starting Beta of 1 for exp. annealing
beta_final_exp = 30 # we chose a final Beta of 30 for exponential annealing
beta_initial_linear = 0.1 # we chose starting Beta of 0.1 for lin. annealing
beta_final_linear = 12 # we chose a final Beta of 12 for linear annealing

# computing the constant factors for both of our methods:
# - linear: (beta_final - beta_initial) / (N - 1) 
# - exponential: (beta_final / beta_initial) ^ (1/N)
constant_linear_factor = (beta_final_linear - beta_initial_linear) / (N - 1)
constant_exp_factor = (beta_final_exp / beta_initial_exp) ** (1/N)

# initialising a matrix of zeros of size r+1 x r+1
v <- matrix(0, nrow = r+1, ncol = r+1)

# v(0, j) for j = 1, ..., 10
for (j in 1:r) {
  # row 1 is city 0, col j+1 is city j
  # generating this separately enables
  # us to ensure that the first column
  # stays empty (i.e. the value of coming
  # back to city 0 is 0)
  v[1, j+1] <- runif(1)
}

# v(i, j) for i = 1, ..., 10 and j ≠ i
for (i in 1:r) {
  for (j in 1:r) {
    if (i != j) {
      v[i+1, j+1] <- runif(1)
    }
  }
}

# We create a function to calculate the total value
V <- function(x,v) {
  # We first add v(0,j) to our total value
  # note: we must add 1 to the value of x given
  # the matrix size is r + 1 (i.e. 11 in our case)
  total <- v[1, x[1]+1]
  
  # we then iterate through the 10 values
  # of the permutation vector and assign 
  # the relevant value of the itinerary i to
  # i+1 to the total from the value matrix
  for (i in 1:(length(x)-1)) {
    total <- total + v[x[i]+1, x[i+1]+1]
  }
  return (total)
}

# Set up plot area for our annealing histograms
par(mfrow = c(1, 2))


# ==========
# Step 2: Simulating the N iterations for each
# annealing method and print histograms

# We create a loop for each annealing method
for (method in 1:2){

# We initialise our beta accordingly to the method (first linear then
# exponential)
if (method == 1) {
  beta = beta_initial_linear
} else {
  beta = beta_initial_exp
}
  
# We start each iteration with an arbitrary 
# permutation 1,...,10
X_current <- c(1:r)

# we store the value of this first permutation
X <- V(X_current,v)

# we create a vector for each value of each state
X = c(X)

if (method == 1) {
  name = "Linear"
} else {
  name = "Exponential"
}
  
# We now simulate all N iterations of our MC
for (n in 2:N){
  # The first thing we do in our loop is to generate a state from the 
  # proposal chain. Here, we set the proposal chain to be equal to our
  # current itinerary, and we pick two indices i and j uniformly at random
  # from 1,...,r (j and i will be equal about 10% of cases, we do not check for
  # this). Then, we swap values Y[i] and Y[j] in the proposal chain, hence, 
  # creating a new permutation. Since we do not violate any properties of the 
  # permutation, then y is always a valid permutation in our set C (of 10!
  # possible permutations) and, thus, a feasible proposal.
  
  Y <- X_current # We start by initialising Y as the current chain
  U1 = runif(1) # generate random uniform
  U2 = runif(1) # generate random uniform
  i = floor(U1*r)+1 # draw uniformly from {1,2,...,r}
  j = floor(U2*r)+1 # draw uniformly from {1,2,...,r}

  # We do not ensure that j and i are always different:
  # We could have done this by generating random uniforms until j is different 
  # from i i.e. we use rejection sampling to ensure that we select two distinct 
  # indices. We would have used the below loop:
  # while (j == i) {
  #   U2 = runif(1)
  #   j = floor(U2*r)+1
  # }
  # In our case, repeating the permuations is deemed acceptable
  
  # we perform the swap to create a new and valid permutation
  temp <- Y[i]
  Y[i] <- Y[j]
  Y[j] <- temp
  
  # As mentioned, we do not need to check for feasibility of Y as our choice
  # is already a valid permutation in our set of permutations C.
  
  # We now calculate the value of the current itinerary 
  # and the proposed itinerary
  V_current = V(X_current,v)
  V_prop = V(Y,v)
  
  # We calculate the acceptance probability
  # Note: we use min between the exponential and 1 because if the proposal chain
  # has a higher value than the current (i.e. the exponential term is greater
  # than 1), we accept it with probability 1.
  exp_term = exp(beta*(V_prop - V_current))
  prob_accept = min(exp_term,1) 
  
  # We generate the random uniform for the acceptance probability
  U3 = runif(1)
  
  if (U3 < prob_accept){ # we accept
    X_current = Y
  }
  # if we reject Y then X_current stays the same so we do nothing
  
  # We store the value of the current chosen itinerary in our X vector
  # (which we later use to plot the stationary distribution).
  X = c(X,V(X_current, v))
  
  if (method == 1){
    beta <- beta + constant_linear_factor 
  } else {
    beta <- beta * constant_exp_factor
  }
  
} # end of steps

# We incorporate the line plot within the beta loop so as to ensure
# that one line plot is plotted for each value of beta
# Note: we plot the 10000 values to show that the MC explores the state space 
# initially and then converges towards the stationary distribution at the end
plot(X, type="l", xlab="Iterations", ylab="States", 
     main=paste(name,"Annealing \n (with mean =",
                round(mean(X[(N-L+1):N]),digits=2),
                " in the last", L, "iterations)"))

# We also compute the average of the distribution over the last L=1000 values
print(paste("Average of the last", L, "values with", name,"Annealing", "=",
            mean(X[(N-L+1):N])))

# We add the maximum value found by the simulation
print(paste("Max of", round(max(X),digits=2), "with", name,"Annealing"))


} # end of loop for annealing methods 

# reset the plot area to avoid errors
par(mfrow = c(1, 1))