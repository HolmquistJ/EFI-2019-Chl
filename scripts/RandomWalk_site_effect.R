#### Data Model
  model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dnorm(mu[i], tau_obs)
  }

   #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(x[i],tau_add) #mu's here are on log scale
    x[i] <- x[i-1] + site[site_no[i]]
  }
  
  #### Priors
  x[1] ~ dgamma(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  tau_obs ~ dgamma(a_obs, r_obs)
  mu[1] ~ dgamma(0.01,0.01)
  tau_site ~ dgamma(0.01,0.01)
  
  #Loops through number of years and defines prior for each year 
  for(i in 1:max_site) {
    site[i] ~ dnorm(0,tau_site)
  }
}
