# This function generates a normal(mu,sigma^2) random variable 
# by first generating an |Z|, then Z, and then X = mu+sigma(Z)
# We generate |Z| via the rejection method using the exp(1) rv.

#It takes as input the mean and s.d.

gen_normal = function(mu, sigma){
  
  # First we generate |Z|:
  
  # Generate an exp(1) RV via the Uniform(0,1)
  U1 = runif(1)
  Y = -log(U1)
  
  # for the rejection method
  U2 = runif(1)
  counter = 1
  
  while(U2 > exp((-(Y-1)^2)/2)){
    U1 = runif(1)
    Y = -log(U1)
    U2 = runif(1)
    counter = counter + 1
  }
  
  Z_abs= Y
  
  # To make this into a Z RV we set it to Y and -Y with equal probability
  U3 = runif(1)
  if (U3 <= 0.5){
    Z = Y
  } else{ 
    Z = -Y
  }
  
  # To make this into a X~N(mu,sigma^2) 
  
  X = mu + sigma * Z

  return(X)
}