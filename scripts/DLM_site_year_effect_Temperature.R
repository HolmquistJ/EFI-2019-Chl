model{
  
  #looping through sites first
  for(i in 1:max_site){
    
    #### Data Model
    for(j in 1:N){
      #this fits the model to your observed data. 
      y[i,j] ~ dnorm(mu[i,j], tau_obs)
      Temp[i,j] ~ dnorm(mu_T[i,j],tau_obs_T)
    }
    
    #### Process Model
    
    
    for (j in 2:N){
      #this adds process error
      mu[i,j]~dnorm(x[i,j],tau_add) #mus are the latent state (true chl-a)
      
      #this is the process model with a covariate and random year and site effects
      x[i,j] <- beta[1] + beta[2]*x[i,j-1] + beta[3]*Temp[i,j] + yr[year_no[j]] + site[site_no[i]]
      
      #process model for temperature
      mu_T[i,j]~dnorm(mo_avg[j],tau_add_T)
      
    }
    
    #setting initial conditions for things that are needed in the 2:N loop
    x[i,1] ~ dgamma(x_ic,tau_ic) 
    mu[i,1] ~ dgamma(0.01,0.01)
    mu_T[i,1] ~ dgamma(0.01,0.01)
    
    #defining prior for each site's site effect
    site[i] ~ dnorm(0,tau_site)

  }
  
  #### Priors
  tau_add ~ dgamma(a_add,r_add) #precision for process error
  tau_obs ~ dgamma(a_obs, r_obs) #precision for observation error
  tau_yr ~ dgamma(0.01,0.01) #precision for year effect
  tau_site ~ dgamma(0.01,0.01) #precision for site effect
  beta ~ dmnorm(beta.m,beta.v) #priors for linear process coefficients
  tau_obs_T ~ dgamma(0.01, 0.01) #precision for temperature prior
  tau_add_T ~ dgamma(0.01, 0.01) #precision for temperature prior
  
  #Loops through number of years and defines prior for each year 
  for(j in 1:max_year) {
    yr[j] ~ dnorm(0,tau_yr)
  }
  
}

