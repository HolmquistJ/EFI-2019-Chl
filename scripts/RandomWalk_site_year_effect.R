#### Data Model
  model{
    
  for(i in 1:max_site){
  
    for(j in 1:max_year){
    #this fits the model to your observed data. 
    y[i] ~ dnorm(mu[i], tau_obs)
  }

   #### Process Model
  
    
    for (j in 2:max_year){
    mu[i,j]~dnorm(x[i,j],tau_add) #mus are the latent state (true chl-a)
    x[i,j] <- x[i,j-1] + yr[year_no[j]] + site[site_no[i]]
    }
  }
  
  #### Priors
  x[i,1] ~ dgamma(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  tau_obs ~ dgamma(a_obs, r_obs)
  mu[i,1] ~ dgamma(0.01,0.01)
  tau_yr ~ dgamma(0.01,0.01)
  tau_site ~ dgamma(0.01,0.01)
  
  #Loops through number of years/sites and defines prior for each one 
  for(j in 1:max_year) {
    yr[j] ~ dnorm(0,tau_yr)
  }
  
  for(i in 1:max_site) {
    site[i] ~ dnorm(0,tau_site)
  }
}
