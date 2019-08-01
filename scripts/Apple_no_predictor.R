RandomWalk = "
model{

#### Data Model
for(t in 1:n){
y[t] ~ dnorm(x[t],tau_obs)
}

#### Process Model
for(t in 2:(n+10)){
x[t]~dnorm(x[t-1],tau_add)
}

#### Priors
x[1] ~ dnorm(log(x_ic),tau_ic)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"

dat <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>%
  filter(Name == "Apple") %>%
  mutate(date = ymd(paste(year, month, 15, sep="-"))) %>% 
  arrange(date)

y <- dat$CHLcal

data <- list(y=log(y),
n=length(y),
x_ic=1,
tau_ic=100,
a_obs=1,
r_obs=1,
a_add=1,
r_add=1)

nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),tau_obs=5/var(log(y.samp)))
} #inits are for each data point (or lag), so there are 620 (or 619) inits?


j.model   <- jags.model (file = textConnection(RandomWalk),
data = data,
inits = init,
n.chains = 3)



jags.out   <- coda.samples (model = j.model,
variable.names = c("tau_add","tau_obs","x"),
n.iter = 1000)

ef.out <- ecoforecastR::fit_dlm(model=list(obs="y",fixed=""),data)


out <- as.matrix(ef.out$predict)

out_tibble <- as_tibble(out) %>% 
  clean_names() %>% 
  #gather(key,value,c(-tau_add,-tau_obs)) %>% 
  gather(key,value) %>% 
  separate(key,c("delete","date_id"),sep="_")

dat_merge <-dat %>% 
  rownames_to_column(var="date_id") %>% 
  add_row(date_id = c(206:216))

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
  add_row(date_id = c(206:216))

ggplot()+
  geom_line(data=out_summary, aes(x=date_id, y=median),color="dodgerblue")+
  geom_point(data = dat_plot, aes(x=date_id, y=CHLcal),color="black")+
  geom_ribbon(data = out_summary, aes(x=date_id, ymin=low95,ymax=high95),
              alpha=0.2,fill="dodgerblue")+
  coord_cartesian(ylim=c(0,200))

  