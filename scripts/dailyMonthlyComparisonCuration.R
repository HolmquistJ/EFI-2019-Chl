# Buoy Data Curation
library(tidyverse)
library(readxl)
library(lubridate)

headerNames <- names(read_delim("data/original/GREON 03_WQData (1) (2).dat", delim=",", skip=1))

buoy <- read_delim("data/original/GREON 03_WQData (1) (2).dat", delim=",",
                   skip=4, col_names = F)

names(buoy) <- headerNames

# Load up table
buoyMonthlySum <- buoy %>%
  select(TIMESTAMP, YSI_Chl_ugL) %>%  # Select datetime and CHL
  mutate(YSI_Chl_ugL = ifelse(YSI_Chl_ugL=="NAN", NA, as.numeric(YSI_Chl_ugL)),
         year=year(TIMESTAMP), # Sepearate YMD as strings
         month=month(TIMESTAMP),
         day=day(TIMESTAMP),
         dims = days_in_month(ymd_hms(as.character(TIMESTAMP)))[[1]]) %>%
  filter(TIMESTAMP >= ymd("2015-07-01")) %>% # Get max day
  group_by(year, month) %>%
  mutate(maxDay = max(day),
         minDay = min(day)) %>% 
  filter(complete.cases(YSI_Chl_ugL),
         maxDay>=dims,
         minDay == 1) %>%
  mutate(midMonthSampleChl = first(YSI_Chl_ugL[which(day==15)])) %>%
  filter(complete.cases(midMonthSampleChl))

plot(log(buoyMonthlySum$midMonthSampleChl), 
     log(buoyMonthlySum$YSI_Chl_ugL))

plot(buoyMonthlySum$midMonthSampleChl, 
     buoyMonthlySum$YSI_Chl_ugL)

write_csv(buoyMonthlySum, "data/derivative/buoyMonthlySum.csv")
