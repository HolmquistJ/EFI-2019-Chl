library(readxl)
library(lubridate)
library(rjags)
library(coda)
library(tidyverse)
library(janitor)
library(ecoforecastR)

Trib_Q_data <- read_excel("data/original/Trib_Q_data.xlsx") %>% 
  gather(key,discharge,c(-Month,-Year)) %>% 
  separate(key,c("Name","q","units")) %>% 
  mutate(date = ymd(paste(Year,Month,15,sep="-"))) %>% 
  select(discharge,date,Name,units)

dat_chl <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>%
  mutate(date = ymd(paste(year, month, 15, sep="-"))) %>% 
  arrange(date)


#merge chl and discharge, filter to one site
dat <- dat_chl %>% 
  left_join(Trib_Q_data) %>% 
  filter(Name=="Apple") %>% 
  rownames_to_column() %>% 
  add_row(rowname = 203:223) %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  mutate(y_holdout = ifelse(rowname >=163, NA, CHLcal))


y <- dat$y_holdout
x <- dat$discharge
data <- list(y=log(y),
             n=length(y),
             x_ic=1,
             tau_ic=100,
             a_obs=.1,
             r_obs=.1,
             a_add=.1,
             r_add=.1)

RandomWalk = "
model{

#### Data Model
for(t in 1:n){
y[t] ~ dnorm(x[t],tau_obs)
}

#### Process Model
for(t in 2:n){
x[t]~dnorm(x[t-1],tau_add)
}

#### Priors
x[1] ~ dnorm(log(x_ic),tau_ic)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"


j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         n.chains = 3)



jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs","x"),
                            n.iter = 1000)

#plot(jags.out)

out_rw <- as.matrix(jags.out)

out_tibble <- as_tibble(out_rw) %>% 
  clean_names() %>% 
  gather(key,value,c(-tau_add,-tau_obs)) %>% 
  #gather(key,value) %>% 
  separate(key,c("delete","date_id"),sep="_")

dat_merge <-dat %>% 
  rownames_to_column(var="date_id")

out_summary <- out_tibble %>% 
  left_join(dat_merge) %>% 
  mutate(date_id = as.numeric(date_id)) %>% 
  group_by(date_id) %>%  
  mutate(value = exp(value)) %>% 
  summarize(median = median(value),
            low95 = quantile(value, probs=0.025),
            high95 = quantile(value, probs=0.975))

dat_plot <- dat %>% 
  arrange(date) %>% 
  rownames_to_column(var="date_id") %>% 
  mutate(date_id = as.numeric(date_id))%>% 
  add_row(date_id = c(203:223))

apple_forecast_Rwalk <- ggplot()+
  geom_line(data=out_summary, aes(x=date_id, y=median),color="dodgerblue")+
  geom_point(data = dat_plot, aes(x=date_id, y=CHLcal),color="black")+
  geom_ribbon(data = out_summary, aes(x=date_id, ymin=low95,ymax=high95,
                                      fill = date_id>163),
              alpha=0.5)+
  scale_fill_brewer(type="qual", palette = 6, direction=-1)+
  guides(fill=F)+
  coord_cartesian(ylim=c(0,200))+
  ggtitle("Random Walk - Apple")+
  ylab("Chlorophyll a")

apple_forecast_Rwalk

  