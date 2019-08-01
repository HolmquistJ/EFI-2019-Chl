library(rjags)
library(ggmcmc)
library(tidyverse)
library(lubridate)

dailyMonthComp <- read_csv("data/derivative/buoyMonthlySum.csv")

monthlySamples <- dailyMonthComp %>% 
  group_by(year, month, midMonthSampleChl) %>%
  summarise(dayCount = length(unique(day)))

monthlySampleDates <- dailyMonthComp %>%
  group_by(year, month, day) %>%
  summarise() %>%
  select(year, month, day)
  
monthlySampleDates$sampleIndex <- 1:nrow(monthlySampleDates)

monthlySamples$index <- 1:nrow(monthlySamples)

monthlySamples$DQindex <- ifelse(monthlySamples$index==1, monthlySamples$dayCount, cumsum(monthlySamples$dayCount))
monthlySamples$DQindexStart <- ifelse(monthlySamples$index==1, 1, lag(monthlySamples$DQindex+1))

data <- list(monthlyChl = log(dailyMonthComp$midMonthSampleChl),
                  dailyChl = log(dailyMonthComp$YSI_Chl_ugL),
                  N = nrow(dailyMonthComp),
                  J = nrow(monthlySamples),
                  chlThreashold = 30,
                  dqIndexStart = monthlySamples$DQindexStart,
                  dqIndex = monthlySamples$DQindex,
                  dqMonthlySamps = log(monthlySamples$midMonthSampleChl))

chlObsModule <- "model{

# priors 
tau.1 ~ dgamma(0.001,0.001)

for (i in 1:N) {
  dailyChl[i] ~ dnorm(monthlyChl[i], tau.1)
}

for (j in 1:J) {
  for (i in dqIndexStart[j]:dqIndex[j]) {
    chlDraw[i] ~ dnorm(dqMonthlySamps[j], tau.1) 
    drawExceeded[i] <- ifelse(exp(chlDraw[i])>chlThreashold, 1, 0)
  }
}

}"

j.model <- jags.model(file = textConnection(chlObsModule),
                      data=data,
                      n.chains = 3)

jags.out <- coda.samples(model=j.model, variable.names=c("tau.1", "drawExceeded", "chlDraw"),
                         n.iter=5000)

tidyJags<- ggs(jags.out)

tidyJagsParams <- tidyJags %>%
  filter(grepl("tau.1", Parameter))

ggs_traceplot(tidyJagsParams)
ggs_density(tidyJagsParams)

tidyJagsDQs <- tidyJags %>%
  filter(! grepl("tau.1", Parameter)) %>%
  separate(Parameter, into = c("Parameter", "sampleIndex")) %>%
  mutate(sampleIndex=as.numeric(sampleIndex)) %>% 
  arrange(Iteration, Chain, sampleIndex, Parameter)

daysExceeded <- tidyJagsDQs %>%
  filter(Parameter == "drawExceeded") %>%
  left_join(monthlySampleDates)  %>%
  group_by(Iteration, Chain, year, month) %>%
  summarise(nOfEvents = sum(value)) %>%
  group_by(year, month) %>%
  summarise(upperCI = quantile(nOfEvents, 0.975),
            median = median(nOfEvents),
            lowerCI = quantile(nOfEvents, 0.025)) %>%
  mutate(displayDate = ymd(paste(year, month, as.character(15), sep="-")))

daysExceededGathered <- daysExceeded %>%
  gather(key="interval", value="exceedence events", upperCI, lowerCI)

ggplot(data=daysExceeded, aes(x=displayDate)) +
  geom_point(aes(y=median)) +
  geom_segment(aes(y=lowerCI, yend=upperCI, xend=displayDate))
