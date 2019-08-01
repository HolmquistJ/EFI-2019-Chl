#River data viz
#Author: Mary Lofton
#Date: 30JUL19

library(tidyverse)
library(lubridate)
library(zoo)
library(rjags)
library(runjags)

#load data and get in correct format
dat <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated_wideForm.csv") %>%
  select(-fracYear) %>%
  mutate(date = ymd(paste(year, month, 15, sep="-"))) %>%
  select(date, Apple:Wapsipinicon) %>%
  filter(year(date) != 1998) %>%
  select(-date)
  
#identify response variable and look at histogram
y <- t(as.matrix(dat))
hist(y)

#data wrangling to get year_no for year effect and site_no for site effect
years <- c(1999:2018)
year_no = rep(as.numeric(as.factor(years)),each = 12)

dat1 <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>%
  arrange(Name)
sites <- unique(dat1$Name)
site_no = as.numeric(as.factor(sites))

####Model - MUST MANUALLY TYPE IN THE CORRECT MODEL NAME HERE!!!
model_name = 'RandomWalk_site_year_effect' 
model=paste0("scripts/",model_name, '.R') #Do not edit

###List correct inputs to JAGS for each model
#Random Walk w/ year effect
data.RandomWalk_year_effect <- list(y=y, N=length(y),x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10,  year_no=dat2$year_no, max_year = 21)
variable.names.RandomWalk_year_effect<- c("tau_add", "tau_obs", "tau_yr")
variable.namesout.RandomWalk_year_effect<- c("tau_add", "mu", "tau_obs", "tau_yr")
init.RandomWalk_year_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_yr = 0.001), list(tau_add=0.1, tau_obs = 0.1,  tau_yr = 0.1), list(tau_add=1, tau_obs = 1,  tau_yr = 1))
params.RandomWalk_year_effect <- c("tau_add", "tau_obs","tau_yr")

#Random Walk w/ site effect
data.RandomWalk_site_effect <- list(y=y, N=length(y),x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10, site_no=dat2$site_no, max_site = 9)
variable.names.RandomWalk_site_effect<- c("tau_add", "tau_obs", "tau_site")
variable.namesout.RandomWalk_site_effect<- c("tau_add", "mu", "tau_obs", "tau_site")
init.RandomWalk_site_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_site = 0.001), list(tau_add=0.1, tau_obs = 0.1, tau_site = 0.1), list(tau_add=1, tau_obs = 1, tau_site = 1))
params.RandomWalk_site_effect <- c("tau_add", "tau_obs","tau_site")


#Random Walk w/ site-year effect
data.RandomWalk_site_year_effect <- list(y=y,x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10, site_no=site_no, max_site = 9, year_no=year_no, max_year = 20)
variable.names.RandomWalk_site_year_effect<- c("tau_add", "tau_obs", "tau_site", "tau_yr")
variable.namesout.RandomWalk_site_year_effect<- c("tau_add", "mu", "tau_obs", "tau_site","tau_yr")
init.RandomWalk_site_year_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_site = 0.001, tau_yr = 0.001), list(tau_add=0.1, tau_obs = 0.1, tau_site = 0.1, tau_yr = 0.1), list(tau_add=1, tau_obs = 1, tau_site = 1, tau_yr = 1))
params.RandomWalk_site_year_effect <- c("tau_add", "tau_obs","tau_site","tau_yr")

#get inputs to jags based on the model name you selected above
data = eval(parse(text = paste0('data.', model_name)))
variable.names = eval(parse(text = paste0('variable.names.', model_name)))
variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
init = eval(parse(text = paste0('init.', model_name)))
params = eval(parse(text = paste0('params.', model_name)))

jags_plug_ins <- list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params) 



#define model and initialize
j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = jags_plug_ins$init.model,
                         n.chains = 3)

#run model
jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     burnin =  2000, 
                     sample = 5000, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#identify params
params <- jags_plug_ins$params.model

#look at param traceplots
for (i in 1:length(params)){
  #png(file=file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
  plot(jags.out, vars = params[i]) 
  #dev.off()
}

#get a matrix to work w/ for predictive intervals, etc.
jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

