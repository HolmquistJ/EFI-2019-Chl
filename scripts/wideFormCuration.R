library(tidyverse)

chl <- read_csv("data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>% 
  mutate(fracYear = year + month/12) %>% # convert time to fractional years
  select(Name, year, month, fracYear, CHLcal)

targetYearDays <- expand.grid(year=min(chl$year):max(chl$year),
                              month=1:12) %>%
  arrange(year, month) %>%
  mutate(fracYear = year + month/12,
         Name=NA, CHLcal=NA) %>%
  filter(! fracYear %in% chl$fracYear)

chlWide <- chl %>%
  bind_rows(targetYearDays) %>%
  #select(Name, fracYear, CHLcal) %>%
  spread(key=Name, value=CHLcal)

write_csv(chlWide, "data/derivative/UMR_chl_tribs_bymonth_curated_wideForm.csv")

