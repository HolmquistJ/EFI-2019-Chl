library(tidyverse)

chl <- read.csv("data\derivative\UMR_alldata_tribs_bymonth_curated") %>% 
  mutate(fracYear = year + month/12) %>% # convert time to fractional years
  select(Name, year, month, fracYear, CHLcal, TN)

targetYearDays <- expand.grid(year=min(chl$year):max(chl$year),
                              month=1:12) %>%
  arrange(year, month) %>%
  mutate(fracYear = year + month/12,
         Name=NA, CHLcal=NA) %>%
  filter(! fracYear %in% chl$fracYear)

chlWide <- chl %>%
  bind_rows(targetYearDays) %>%
  #select(Name, fracYear, CHLcal) %>%
  spread(key=Name, value=CHLcal) %>%
  select(-"<NA>")

chlWide.m<-as.matrix(chlWide)
chlWide.m<-chlWide.m[,4:12]

#########################nitrogen###################################################

TN.m <- tbl_df(CHL.df) %>% 
  mutate(fracYear = year + month/12) %>% # convert time to fractional years
  select(Name, year, month, fracYear, TN)

targetYearDays <- expand.grid(year=min(TN.m$year):max(TN.m$year),
                              month=1:12) %>%
  arrange(year, month) %>%
  mutate(fracYear = year + month/12,
         Name=NA, TN=NA) %>%
  filter(! fracYear %in% TN.m$fracYear)

TNWide <- TN.m %>%
  bind_rows(targetYearDays) %>%
  #select(Name, fracYear, CHLcal) %>%
  spread(key=Name, value=TN) %>%
  select(-"<NA>")

TNWide.m[is.na(TNWide.m)] <- 4

#########################################temp########################################

temp <- tbl_df(CHL.df) %>% 
  mutate(fracYear = year + month/12) %>% # convert time to fractional years
  select(Name, year, month, fracYear, TEMP)

targetYearDays <- expand.grid(year=min(temp$year):max(temp$year),
                              month=1:12) %>%
  arrange(year, month) %>%
  mutate(fracYear = year + month/12,
         Name=NA, CHLcal=NA) %>%
  filter(! fracYear %in% chl$fracYear)

TNWide <- TN %>%
  bind_rows(targetYearDays) %>%
  #select(Name, fracYear, CHLcal) %>%
  spread(key=Name, value=TN) %>%
  select(-"<NA>")

TNWide.m<-as.matrix(TNWide)
TNWide.m<-TNWide.m[,5:13]
