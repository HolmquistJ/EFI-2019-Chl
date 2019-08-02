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

dat[229:240,] <- NA

Temp <- read_csv("./data/derivative/UMR_temp_tribs_bymonth_curated_wideForm.csv") %>%
  select(-fracYear) %>%
  mutate(date = ymd(paste(year, month, 15, sep="-"))) %>%
  select(date, Apple:Wapsipinicon) %>%
  filter(year(date) != 1998) %>%
  select(-date)
  
#identify response variable and look at histogram
y <- t(as.matrix(dat))
hist(y)
N = ncol(y)

#identify covariate and look at histogram
Temp <- t(as.matrix(Temp))
Temp_NA <- is.na(Temp)
Temp_NA[,1:240] <- as.numeric(Temp_NA[,1:240])

hist(Temp)

#data wrangling to get year_no for year effect and site_no for site effect
years <- c(1999:2018)
year_no = rep(as.numeric(as.factor(years)),each = 12)

dat1 <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>%
  arrange(Name)
sites <- unique(dat1$Name)
site_no = as.numeric(as.factor(sites))

#set up temp by month vector
temp_mo <- read_csv("./data/derivative/UMR_alldata_tribs_bymonth_curated.csv") %>%
  select(Name, month, TEMP) %>%
  group_by(month) %>%
  summarize(temp_avg = mean(TEMP))
mo_avg <- rep(temp_mo$temp_avg,20)

#month_no <- rep(c(1:12),20)

####Model - MUST MANUALLY TYPE IN THE CORRECT MODEL NAME HERE!!!
model_name = 'DLM_site_year_effect_Temperature' 
model=paste0("scripts/",model_name, '.R') #Do not edit

###List correct inputs to JAGS for each model
# #Random Walk w/ year effect
# data.RandomWalk_year_effect <- list(y=y, N=length(y),x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10,  year_no=dat2$year_no, max_year = 21)
# variable.names.RandomWalk_year_effect<- c("tau_add", "tau_obs", "tau_yr")
# variable.namesout.RandomWalk_year_effect<- c("tau_add", "mu", "tau_obs", "tau_yr")
# init.RandomWalk_year_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_yr = 0.001), list(tau_add=0.1, tau_obs = 0.1,  tau_yr = 0.1), list(tau_add=1, tau_obs = 1,  tau_yr = 1))
# params.RandomWalk_year_effect <- c("tau_add", "tau_obs","tau_yr")
# 
# #Random Walk w/ site effect
# data.RandomWalk_site_effect <- list(y=y, N=length(y),x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10, site_no=dat2$site_no, max_site = 9)
# variable.names.RandomWalk_site_effect<- c("tau_add", "tau_obs", "tau_site")
# variable.namesout.RandomWalk_site_effect<- c("tau_add", "mu", "tau_obs", "tau_site")
# init.RandomWalk_site_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_site = 0.001), list(tau_add=0.1, tau_obs = 0.1, tau_site = 0.1), list(tau_add=1, tau_obs = 1, tau_site = 1))
# params.RandomWalk_site_effect <- c("tau_add", "tau_obs","tau_site")


#Random Walk w/ site-year effect
data.RandomWalk_site_year_effect <- list(y=y,N = N, x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10, site_no=site_no, max_site = 9, year_no=year_no, max_year = 20)
variable.names.RandomWalk_site_year_effect<- c("tau_add", "tau_obs", "tau_site", "tau_yr")
variable.namesout.RandomWalk_site_year_effect<- c("tau_add", "mu", "tau_obs", "tau_site","tau_yr")
init.RandomWalk_site_year_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_site = 0.001, tau_yr = 0.001), list(tau_add=0.1, tau_obs = 0.1, tau_site = 0.1, tau_yr = 0.1), list(tau_add=1, tau_obs = 1, tau_site = 1, tau_yr = 1))
params.RandomWalk_site_year_effect <- c("tau_add", "tau_obs","tau_site","tau_yr")

#DLM w/ site-year effect
data.DLM_site_year_effect_Temperature <- list(y=y,N = N, mo_avg = mo_avg, Temp = Temp, beta.m=as.vector(c(0,0,0)), beta.v=solve(diag(1E-03,3)), x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10, site_no=site_no, max_site = 9, year_no=year_no, max_year = 20)
variable.names.DLM_site_year_effect_Temperature<- c("tau_add", "tau_obs", "tau_site", "tau_yr", "beta[1]","beta[2]","beta[3]", "tau_obs_T","tau_add_T")
variable.namesout.DLM_site_year_effect_Temperature<- c("tau_add", "mu", "tau_obs", "tau_site","tau_yr", "beta[1]","beta[2]","beta[3]", "tau_obs_T","tau_add_T", "mu_T")
init.DLM_site_year_effect_Temperature <- list(list(tau_add=0.01, tau_obs = 0.01, tau_site = 0.01, tau_yr = 0.01,tau_obs_T = 0.01, tau_add_T = 0.01,beta=c(-0.5,-0.5,-0.5)), list(tau_add=0.1, tau_obs = 0.1, tau_site = 0.1, tau_yr = 0.1, tau_obs_T = 0.1, tau_add_T = 0.1,beta=c(0,0,0)), list(tau_add=1, tau_obs = 1, tau_site = 1, tau_yr = 1,tau_obs_T = 1, tau_add_T = 1,  beta=c(0.5,0.5,0.5)))
params.DLM_site_year_effect_Temperature <- c("tau_add", "tau_obs","tau_site","tau_yr", "beta[1]","beta[2]","beta[3]", "tau_obs_T","tau_add_T")


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
                     sample = 2000, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#identify params
params <- jags_plug_ins$params.model

#look at param traceplots
for (i in 1:length(params)){
  png(file=file.path("C:/Users/Mary Lofton/Desktop/EFI_2019_WG",paste(paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
  plot(jags.out, vars = params[i]) 
  dev.off()
}

#get a matrix to work w/ for predictive intervals, etc.
jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

colnames(out[,2160:2168])

dates <- dat <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated_wideForm.csv") %>%
  mutate(date = ymd(paste(year, month, 15, sep="-")))

time <- as.Date(as.character(dates$date))
times <- time[13:252]
#time.rng = c(1,20) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=grep("mu\\[9,", colnames(out))
mu = out[,mus]
ci <- apply(mu,2,quantile,c(0.025,0.5,0.975))


png(file=file.path("C:/Users/Mary Lofton/Desktop/EFI_2019_WG",paste(paste0(model_name,'_chla_CI.png'), sep = '_')), res=300, width=30, height=15, units='cm')
plot(times,ci[2,],type='n', ylab="Chl-a", ylim = c(min(ci[1,]),max(ci[3,])),main="Obs, Latent CI (blue), PI (green), Obs PI (grey)")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
ciEnvelope(times,obs_pi[1,],obs_pi[3,],col="gray")
ciEnvelope(times,pi[1,],pi[3,],col="Green")
points(times,y[9,],pch="+",cex=0.8)
points(times,ci[2,217:240],pch = 5, cex = 0.8)
dev.off()


