# Clear your environment of variables
rm(list = ls())

# We use Discrete Event Simulation (DES) to simulate  an (s,S) inventory model.
# The demand for the inventory model is homogeneous Poisson with rate lambda. 
# We simulating the Poisson process by generating exp(lambda) and adding them.
# The amount demand by each customer is discrete uniform from {2,3,4}. 
# We simulate this by X = Int(3U)+2.

set.seed(1)
T = 20
lambda = 2
L = 1 # time it takes for an order to be delivered
r = 100 # price each unit is sold
h = 5 #  holding cost per unit per unit time
I = 2 # initial inventory at time 0
s = 10 # reserve inventory level
S = 30 # max inventory level


# For order cost c(y) = 40 + 3y, we use function order_cost.R
# We source this here:
source("C:/Users/PAPADAKK/Dropbox/MA424 - New/Lecture 8/L8 seminar/R code/order_cost.R")
source("C:/Users/PAPADAKK/Dropbox/MA424 - New/Lecture 8/L8 seminar/R code/gen_sample_mean_var.R")

Profit_unit_time = c() # keeps profit entries from each iteration

d = 25.5102
k = 1
k_flag = 1

while(k_flag){
  ########## iterations for averages #################

# Initialize
t= 0 # time
C = 0; # total amount of ordering costs up to time t
H = 0; # total amount of inventory holding costs by time t
R = 0; # total amount of revenue earned by time t

# State variables
x = I # amount of inventory at time t
y = 0 # amount of inventory on order
  # ST will be a matrix that stores our (x,y,t) state-time pairs that we record 
  # every time there is an event: customer arrival or order delivery
ST = matrix(c(x,y,t) , nrow = 1, ncol = 3)

# Event List variables
   # use the nonhomogeneous Poisson to generate t_A = time of next arrival 
U = runif(1)
t_0 = t - (1/lambda)*log(U)  # arrival time of next customer
t_1 = Inf # time at which the order being filled will be delivered; 
          # Inf if no order
event_list =  matrix(c(t_0, t_1), nrow=1, ncol=2)

# Main loop
flag = 1

while(flag){
  
  if (t_0 == min(t_0,t_1,T)) {
######## Case 1:  customer arrival (within T) ############
     
    H = H + (t_0 - t)*x*h # we count the holding cost between t_0 and t
    t = t_0 # move to time t_0
    U = runif(1)
    D = floor(3*U) + 2 # generate amount in {2,3,4} demanded by current customer
    w = min(D,x) # amount of order that can be filled
                 # inventory after filling this order is x-w
    R = R + w*r # count the revenue from this customer
    x = x - w # update inventory 
    
    if ((x < s) & (y == 0)){ 
    ####### if inventory is below the reserve level, and no order ##########
      y = S-x # we order to bring the level up to S
      t_1 = t + L # order will arrive in L time units 
    }
    U = runif(1) 
    t_0 = t - (1/lambda)*log(U) # generate arrival time of next customer
    
    ST = rbind(ST, c(x,y,t)) # update ST
    event_list = rbind(event_list, c(t_0,t_1))
    
  } else if (t_1 == min(t_0,t_1,T)) { 
####### Case 2: order delivered (within T) #############
  
    H = H + (t_1 - t)*x*h # we count the holding cost between t_1 and t
    t = t_1 # move to time t_1
    C = C + order_cost(y) # count to cost of the order that just got delivered
    x = x + y # update inventory level
    y = 0 # assume that we don't need to order if S >= 2s
    t_1 = Inf
    
    ST = rbind(ST, c(x,y,t)) # update ST
    event_list = rbind(event_list, c(t_0,t_1))
    
  } else if (T == min(t_0,t_1,T)){
###### Case 3: time ended, we stop process #######################
    
    H = H + (T - t)*x*h # we count the holding cost between T and t
    t = T
    ST = rbind(ST, c(x,y,t)) # update ST
    flag = 0 # end process
 
  } else {
    print(paste("I should not be here"))
  }
}
# Save the profit from current iteration
Profit_current = (R-C-H)/T

### Here we update the sample mean and sample variance
if (k == 1){
  Profit_bar = Profit_current
  Var_k = 0
  
} else{
  out = gen_sample_mean_var(k-1, Profit_current, Profit_bar, Var_k)
  Profit_bar = out[1]
  Var_k = out[2]
  
}

### We check the stopping conditions
if ((k >= 100) & (sqrt(Var_k/k) < d)){
  k_flag = 0
}
k = k + 1

} # end iterations

k = k - 1 # as we increased k but did not do that iteration
  
### for exercise 3(b) (i)

print(paste("The estimator of P_T that we are 95% confident it is no more than d from its mean = ", round(Profit_bar, digits =4)))
print(paste("The (sample) standard deviation of this estimator is ", round(sqrt(Var_k/k),digits=4)))
print(paste("The number of iterations that I run for this was ", k))

### for exercise 3(b) (ii)

# The 90% confidence interval is: Profit_bar +/- 1.65 sqrt(Var_k/k)

Upper = Profit_bar + 1.65*sqrt(Var_k/k)
Lower = Profit_bar - 1.65*sqrt(Var_k/k)

print(paste("We are 90% confident that  E[P_T] is in the interval [",round(Lower, digits=4),",",round(Upper, digits=4),"]"))

####################################








