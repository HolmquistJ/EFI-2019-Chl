# Inital Data Curation 
library(tidyverse)

monthlyCl <- read_csv("data/original/UMR_chl_tribs_bymonth.csv")

monthlyCl[monthlyCl<0] <- NA

write_csv(monthlyCl, "data/derivative/UMR_chl_tribs_bymonth_curated.csv")


annualCl <- read_csv("data/original/UMR_chl_tribs.csv")

annualCl[annualCl<0] <- NA

write_csv(annualCl, "data/derivative/UMR_chl_tribs.csv_curated.csv")

