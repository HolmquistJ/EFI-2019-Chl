library(tidyverse)

chlWide <- read_csv("data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>% 
  mutate(fracYear = year + month/12) %>% # convert time to fractional years
  select(Name, fracYear, CHLcal) %>%
  spread(key=Name, value=CHLcal)

write_csv(chlWide, "data/derivative/UMR_chl_tribs_bymonth_curated_wideForm.csv")
