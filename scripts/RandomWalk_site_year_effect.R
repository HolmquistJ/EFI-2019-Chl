#### Data Model
  model{
    
  for(i in 1:max_site){
  
    for(j in 1:max_year){
    #this fits the model to your observed data. 
    y[i,j] ~ dnorm(mu[i,j], tau_obs)
      
  }

   #### Process Model
  
    
    for (j in 2:max_year){
    mu[i,j]~dnorm(x[i,j],tau_add) #mus are the latent state (true chl-a)
    x[i,j] <- x[i,j-1] + yr[year_no[j]] + site[site_no[i]]
    
    }
    
    x[i,1] ~ dgamma(x_ic,tau_ic) 
    mu[i,1] ~ dgamma(0.01,0.01)
    site[i] ~ dnorm(0,tau_site)
  }
  
  #### Priors
  tau_add ~ dgamma(a_add,r_add)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_yr ~ dgamma(0.01,0.01)
  tau_site ~ dgamma(0.01,0.01)
  
  #Loops through number of years/sites and defines prior for each one 
  #Loops through number of years and defines prior for each year 
  for(j in 1:max_year) {
    yr[j] ~ dnorm(0,tau_yr)
  }
}
