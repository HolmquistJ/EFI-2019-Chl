#River data viz
#Author: Mary Lofton
#Date: 30JUL19

library(tidyverse)
library(lubridate)
library(zoo)
library(rjags)
library(runjags)

#load data
dat <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>%
  #filter(Name == "Wapsipinicon") %>%
  mutate(date = ymd(paste(year, month, 15, sep="-"))) 
  

ggplot(data = dat, aes(x = date, y = CHLcal)) +
  geom_point(size = 1)+
  theme_bw()

y <- dat$CHLcal
hist(y)

# years <- unique(dat$year)
# year_no = as.numeric(as.factor(years))

sites <- unique(dat$Name)
site_no = as.numeric(as.factor(sites))

#yrz <- tibble(year = years, year_no = year_no)
sitez <- tibble(Name = sites, site_no = site_no)

#dat1 <- left_join(dat, yrz, by = "year")
dat2 <- left_join(dat, sitez, by = "Name")


####Model
model_name = 'RandomWalk_site_effect' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
model=paste0("scripts/",model_name, '.R') #Do not edit


#Random Walk
data.RandomWalk_site_effect <- list(y=y, N=length(y),x_ic=20,tau_ic = 0.01, a_add = 0.001,r_add = 0.001, a_obs = 10, r_obs = 10, site_no=dat2$site_no, max_site = 9)
variable.names.RandomWalk_site_effect<- c("tau_add", "tau_obs", "tau_site")
variable.namesout.RandomWalk_site_effect<- c("tau_add", "mu", "tau_obs", "tau_site")
init.RandomWalk_site_effect <- list(list(tau_add=0.001, tau_obs = 0.001, tau_site = 0.001), list(tau_add=0.1, tau_obs = 0.1, tau_site = 0.1), list(tau_add=1, tau_obs = 1, tau_site = 1))
params.RandomWalk_site_effect <- c("tau_add", "tau_obs","tau_site")

data = eval(parse(text = paste0('data.', model_name)))
variable.names = eval(parse(text = paste0('variable.names.', model_name)))
variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
init = eval(parse(text = paste0('init.', model_name)))
params = eval(parse(text = paste0('params.', model_name)))

jags_plug_ins <- list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params) 




j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = jags_plug_ins$init.model,
                         n.chains = 3)

jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     burnin =  2000, 
                     sample = 5000, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

params <- jags_plug_ins$params.model

for (i in 1:length(params)){
  #png(file=file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
  plot(jags.out, vars = params[i]) 
  #dev.off()
}

jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

