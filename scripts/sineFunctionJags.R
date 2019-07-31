# Sine Function
library(rjags)
library(ggmcmc)
library(tidyverse)

# load up data
chl <- read_csv("data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>% 
  mutate(fracYear = year + month/12) %>% # convert time to fractional years
  select(fracYear, CHLcal) %>%
  filter(complete.cases(.))

# Create a target of monthly 
yTargets <- seq(min(chl$fracYear), max(chl$fracYear), by=0.1)

# Create list of data

chlData <- list(y=chl$CHLcal,
                time = chl$fracYear,
                targetTime = yTargets,
                N = length(time),
                J = length(yTargets))

chl.mod <- "model{
  pi <- 3.1415

  # priors
  a ~ dnorm(0,0.001) # amplitude
  b ~ dnorm(14,0.001) T(0.01,) # should be positive lowest possible value
  phase ~ dnorm(0,1)
  tau ~ dgamma(0.001,0.001)

  for (i in 1:N) {
    # process model
    z[i] <- ((b + a) + a * sin(2*pi*time[i] - 2*pi*phase))

    # likihood
    y[i] ~ dnorm(z[i], tau) T(0.01,)
  } 
  
  for (j in 1:J) {
    z.new[j] <- (b + a) + a * sin(2*pi*targetTime[j] - 2*pi*phase)
    y.new[j] ~ dnorm(z.new[j], tau) T(0.01,)
  }
}
"

j.model <- jags.model(file = textConnection(chl.mod),
                      data=chlData,
                      n.chains = 3)

jags.out <- coda.samples(model=j.model, 
                         variable.names=c("a","b","phase","tau", "z.new", "y.new"),
                         n.iter=5000)


tidyJags <-ggs(jags.out)

tidyJagsParams <- tidyJags %>%
  filter(! grepl("new", Parameter))

ggs_traceplot(tidyJagsParams)


# ggs_traceplot(tidyJagsParams)

# ggmcmc(tidyJagsParams, file="model_simple-diag.pdf", param_page=5)

tidyJagsDQs <- tidyJags %>%
  filter(grepl("y.new", Parameter) | grepl("z.new", Parameter)) %>% 
  separate(Parameter, into = c("Parameter", "timeIndex")) %>%
  mutate(timeIndex=as.numeric(timeIndex),
         value = exp(value)) %>%
  arrange(Iteration, Chain, Parameter, timeIndex) %>%
  group_by(Iteration, Chain, Parameter) %>%
  mutate(time=yTargets) %>%
  ungroup() %>%
  group_by(Parameter, time) %>% 
  summarise(median = median(value),
            UpperCI=quantile(value, 0.975),
            LowerCI=quantile(value, 0.025))

ggplot(data=tidyJagsDQs, aes(x=time)) + 
  #geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI, fill=Parameter), alpha=0.6, color="lightblue") +
  #geom_line(alpha=0.9, aes(y=median, lty=Parameter), lwd=2) +
  geom_point(data=chl, aes(x=fracYear, y=CHLcal)) +
  scale_y_log10()
