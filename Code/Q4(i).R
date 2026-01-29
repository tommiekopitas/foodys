set.seed(1)

# ==========
# Question 4 (i)
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
beta_values = c(0.01,0.1,1) # chosen Betas = 0.01, 0.1, 1

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

# Set up plot area for our histograms
par(mfrow = c(1, length(beta_values)))


# ==========
# Step 2: Simulating the N iterations for each
# beta in the beta_values vector and printing 
# histograms

# Loop for beta values:
for (betas in 1:length(beta_values)) {

# we assign the beta value
beta <- beta_values[betas]
  
# We start each iteration with an arbitrary 
# permutation 1,...,10
X_current <- c(1:r)

# we store the value of this first permutation
X <- V(X_current,v)

# we create a vector for each value of each state
X = c(X)

# We now simulate all N iterations of our MC
for (n in 2:N){
  # The first thing we do in our loop is to generate a state from the 
  # proposal chain. Here, we set the proposal chain to be equal to our
  # current itinerary, and we pick two indices i and j uniformly at random
  # from 1,...,r. We check that j is not equal to i (this happens about 10% of
  # cases given we sample uniformly at random), if it's the case we generate
  # more uniforms and accept the draw once j is different from i. Then, we swap
  # values Y[i] and Y[j] in the proposal chain, hence, creating a new 
  # permutation. Since we do not violate any properties of the permutation and
  # we check that j is different from i, then y is always a valid permutation in
  # our set C (of 10! possible permutations) and,thus, a feasible proposal.
  
  Y <- X_current # We start by initialising Y as the current chain
  U1 = runif(1) # generate random uniform
  U2 = runif(1) # generate random uniform
  i = floor(U1*r)+1 # draw uniformly from {1,2,...,r}
  j = floor(U2*r)+1 # draw uniformly from {1,2,...,r}

  # we ensure that j and i are always different
  # by generating random uniforms until j is different from i
  # i.e. we use rejection sampling to ensure that we select two 
  # distinct indices
  while (j == i) {
    U2 = runif(1)
    j = floor(U2*r)+1
  }
  
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
  
} # end of steps

# We incorporate the histogram plotting within the beta loop so as to ensure
# that one histogram is plotted for each value of beta
# Note: we plot only the 1000 last values which is when our MC will have 
# converged to the stationary distribution
hist(X[(N-L+1):N], breaks = 50, xlab="states", ylab="Frequency", 
     main=paste("Histogram of MC (β=",beta,")"))

# We also compute the average of the distribution over the last L=1000 values
print(paste("Average of the last", L, "values with beta", beta, "=", 
            mean(X[(N-L+1):N])))

} # end of loop for betas

# reset the plot area to avoid errors
par(mfrow = c(1, 1))