#### Data Model
  model{
    
  for(i in 1:max_site){
  
    for(j in 1:N){
    #this fits the model to your observed data. 
    y[i,j] ~ dnorm(mu[i,j], tau_obs)
    Temp[i,j] ~ dnorm(temp[i,j],tau_obs_Temp)
  }

   #### Process Model
  
    
    for (j in 2:N){
    mu[i,j]~dnorm(x[i,j],tau_add) #mus are the latent state (true chl-a)
    x[i,j] <- beta[1] + beta[2]*x[i,j-1] + beta[3]*temp[i,j] + yr[year_no[j]] + site[site_no[i]]
    temp[i,j] ~ dnorm(temp[i,j-1],tau_add_Temp)
    }
    
    x[i,1] ~ dgamma(x_ic,tau_ic) 
    mu[i,1] ~ dgamma(0.01,0.01)
    site[i] ~ dnorm(0,tau_site)
    temp[i,1] ~ dnorm(0.5, 0.6)
  }
  
  #### Priors
  tau_add ~ dgamma(a_add,r_add)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_yr ~ dgamma(0.01,0.01)
  tau_site ~ dgamma(0.01,0.01)
  beta ~ dmnorm(beta.m,beta.v)
  tau_add_Temp ~ dgamma(a_add_Temp,r_add_Temp)
  tau_obs_Temp ~ dgamma(a_obs_Temp, r_obs_Temp)
  
  #Loops through number of years/sites and defines prior for each one 
  #Loops through number of years and defines prior for each year 
  for(j in 1:max_year) {
    yr[j] ~ dnorm(0,tau_yr)
  }
}
