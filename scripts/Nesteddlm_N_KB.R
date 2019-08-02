#Kristin Byrd
#Nested DLM of chlorophyll, nested by site, using nitrogen as a covariate,
#take 1

####################################Data Curation####################################
library(tidyverse)
library(lubridate)
library(zoo)
library(rjags)
devtools::install_github("EcoForecast/ecoforecastR")

# monthlyCl <- read_csv("data/original/UMR_all_tribs_bymonth.csv")
# monthlyCl$CHLcal[monthlyCl$CHLcal<0]<-NA
# 
# CHL.tb <- tbl_df(monthlyCl )%>%
#   mutate(time = ymd(paste(year, month, 15, sep="-")),
#          doy = yday(time))
# 
# chl<-CHL.tb$CHLcal
# TN<-CHL.tb$TN
# 
# sites <- unique(CHL.tb$Name)
# siteN = as.numeric(as.factor(sites))
# sitez <- tibble(Name = sites, site_no = siteN)
# 
# years <- unique(CHL.tb$year)
# year_no = as.numeric(as.factor(years))
# yrz <- tibble(year = years, year_no = year_no)
# 
# CHL.df <- as.data.frame(left_join(CHL.tb, sitez, by = "Name"))
# CHL.df <- left_join(CHL.df, yrz, by = "year")
# CHL.df<-subset(CHL.df, time>="1999-01-01")


######################################THE MODEL################################

Nested_DLM="  
  model{
  
  #### Priors
  
  tau_obs ~ dgamma(a_obs,r_obs) #observation model precision
  tau_add ~ dgamma(a_add,r_add) #process model precision
  
  #### Random Effects ##added if using site effect, for example
  tau_alpha~dgamma(0.1,0.1) #random effect precision
  for(i in 1:9){                  #random effect prior
    alpha[i]~dnorm(0,tau_alpha)
    x[1,i] ~ dnorm(x_ic,tau_ic) #initial condition
  }
  
  #### Fixed Effects
  beta.slope ~ dnorm(0,0.001) #priors for coefficients
  beta.last ~ dnorm(0,0.001)

  #### Data Model
  for(s in 1:9){
    
    for(t in 1:157){   #nrep = number of observations by site
    OBS[t,s] ~ dnorm(x[t,s],tau_obs) #what is x? latent variable
  }
  
}
  #### Process Model
  
  for(s in 1:9){
    
    for(t in 2:157){   #nrep = number of observations by site
      mu[t,s] <- x[t-1,s] + betaTN[t,s]*beta.slope + alpha[s] + x[t-1,s]*beta.last
      x[t,s] ~ dnorm(mu[t,s],tau_add) #process model error distribution
    }
  }
  
}"

OBS<-chlWide.m
data <- list(OBS=chlWide.m,betaTN =TNWide.m, x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10)

nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(OBS,300,replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp)),tau_obs=1/var(y.samp))
}

j.model   <- jags.model (file = textConnection(Nested_DLM),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 1000)
plot(jags.out)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("mu","tau_add","tau_obs"),
                            n.iter = 10000)

##############################MODEL Diagnostics#######################################


time.rng = c(1,length(CHL.df$time)) ## adjust to zoom in and out
out <- as.matrix(jags.out)
x.cols <- grep("^x",colnames(out)) ## grab all columns that start with the letter x
ci <- apply(exp(out[,x.cols]),2,quantile,c(0.025,0.5,0.975)) ## model was fit on log scale

plot(time,ci[2,],type='n',ylab="chl")
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(CHL.df$time,CHL.df$CHLcal,pch="+",cex=0.5)

#