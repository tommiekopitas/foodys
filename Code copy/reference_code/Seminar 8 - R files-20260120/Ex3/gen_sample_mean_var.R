# This function updates sample and mean and sample variance when the new
# simulation value is available.

# It takes as input Z_bar,S_sq,n the sample mean, sample variance and the number
# of simulation observations they are based on (n). It also takes as input the 
# new observation Z.

gen_sample_mean_var = function(n, Z, Z_bar, S_sq){
  
  Z_bar_new = Z_bar + (Z-Z_bar)/(n+1)
  
  S_sq_new = (1-(1/n))*S_sq + (n+1)*(Z_bar_new - Z_bar)^2
  
  return(c(Z_bar_new,S_sq_new))
}