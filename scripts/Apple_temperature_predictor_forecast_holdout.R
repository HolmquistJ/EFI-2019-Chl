library(readxl)
library(lubridate)
library(rjags)
library(coda)
library(tidyverse)
library(janitor)
library(ecoforecastR)
library(cowplot)


UMR_all_tribs_bymonth <- read_csv("data/original/UMR_all_tribs_bymonth.csv") %>% 
  mutate(date = ymd(paste(year,month,15,sep="-"))) %>% 
  select(TEMP,date,Name)

dat_chl <- read_csv("./data/derivative/UMR_chl_tribs_bymonth_curated.csv") %>%
  #filter(Name == "Apple") %>%
  mutate(date = ymd(paste(year, month, 15, sep="-"))) %>% 
  arrange(date)


#merge chl and temperature, filter to one site
dat <- dat_chl %>% 
  left_join(UMR_all_tribs_bymonth) %>% 
  filter(Name=="Apple") %>% 
  rownames_to_column() %>% 
  add_row(rowname = 203:223) %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  mutate(y_holdout = ifelse(rowname >=163, NA, CHLcal))


y <- dat$y_holdout
x <- dat$TEMP
data <- list(y=log(y),
             x = x,
             n=length(y),
             x_ic=1,
             tau_ic=100,
             a_obs=.1,
             r_obs=.1,
             a_add=.1,
             r_add=.1)



ef.out_temperature <- fit_dlm(model=list(obs="y",fixed="1 + x"),data)

#show jags code under the hood - muted
strsplit(ef.out_temperature$model,"\n",fixed = TRUE)[[1]]


out <- as.matrix(ef.out_temperature$predict)

out_tibble <- as_tibble(out) %>% 
  clean_names() %>%
  #gather(key,value, c(-"tau_obs",-"tau_add")) %>% 
  gather(key,value) %>% 
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
  add_row(date_id = c(206:216))


apple_temperature_fore <- ggplot()+
  geom_line(data=out_summary, aes(x=date_id, y=median),color="dodgerblue")+
  geom_point(data = dat_plot, aes(x=date_id, y=CHLcal),color="black")+
  geom_ribbon(data = out_summary, aes(x=date_id, ymin=low95,ymax=high95,
                                      fill = date_id>163),
              alpha=0.5)+
  scale_fill_brewer(type="qual", palette = 6, direction=-1)+
  guides(fill=F)+
  coord_cartesian(ylim=c(0,200))+
  ggtitle("dlm with temperature - Apple")+
  ylab("Chlorophyll a")
  
apple_temperature_fore


apple_compare <- plot_grid(apple_forecast_Rwalk, apple_discharge_fore,apple_temperature_fore, ncol=1)
apple_compare
ggsave(apple_compare, file="apple_compare.svg", dpi=500)



