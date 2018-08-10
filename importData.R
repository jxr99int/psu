library(tidyverse)

weather <- read_csv("data/NCDC-CDO-USC00356750.csv")
travel <- read_csv("data/NHTS2009_dd.csv")

library(readxl)
source("load_data.R")

library(readr)
bike <- read_xlsx("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx")

bikecountsTotal <- gather(bikecounts, key="name", value = "total", na.rm = FALSE)

library(dplyr)
library(lubridate)

tidybikecount <- gather(bikecounts, westbound:total, key="type", value="counts")
tidybikecountNA <- filter(tidybikecount, is.na(counts) ) %>% tally
cleanbikecounts <- filter(tidybikecount, !is.na(counts) )

EBWBbikecounts<- tidybikecount %>% 
  filter(type=="total") %>% 
  select(-type) %>% 
  spread(name, counts)

library(magrittr)

BikeByYear <- cleanbikecounts %>%
  mutate(YR = year(date)) %>%
  group_by(YR) %>%
  summarize(YR_total = sum(counts))

BikeByYRbyBridge <-
cleanbikecounts %>%
  mutate(YR = year(date)) %>%
 group_by(name, YR) %>%
 summarize(annual_total = sum(counts))
 
BikeByMonth <-
cleanbikecounts %>%
  mutate(MTH = month(date)) %>%
 group_by(name, MTH) %>%
   summarize(monthly_total = sum(counts))

BikeByWK <- cleanbikecounts %>%
  mutate(WK = week(date)) %>%
group_by(name, WK) %>%
  summarize(week_total = sum(counts))

CleanBikeWeather <- cleanbikecounts %>% 
  mutate(DATE=as_date(date)) %>% 
  left_join(weather)

cleanbikecounts %>%
  group_by(name) %>%
  mutate(rank=rank(desc(counts))) %>%
           filter(rank <=3) %>%
           top_n (3)

travel %>% 
  summarize(TotalMiles = sum(TRPMILES)) 

travel %>% 
  mutate(driving = ifelse(TRPTRANS %in% c("01","02","03","04","05","06","07"), 1,0),
         driving = ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA , driving)) %>%
  filter(driving ==1) %>%
  #group_by(HOUSEID) %>%
  group_by(HOUSEID, PERSONID) %>%
  summarize(TotDrivingMiles = sum(TRPMILES)) 

travelByInc <- travel %>% 
  mutate(income_cat=case_when(
  HHFAMINC %in% c("01", "02", "03", "04", "05", "06") ~ "low income",
  HHFAMINC %in% c("07", "08", "09", "10", "11", "12") ~ "med income",
  HHFAMINC %in% c("13", "14", "15", "16", "17", "18") ~ "high income",
  TRUE ~ as.character(NA))
  ) %>% 
  group_by (income_cat) %>% 
  summarize(mean(TRPMILES))

ggplot(data = cleanbikecounts %>% filter(name %in% c("Hawthorne", "Tilikum") ) %>%
  mutate(YR = year(date)) %>%
  mutate(MTH = month(date)) %>%
  mutate(WK = week(date))) +
  #geom_smooth(mapping = aes(x = YR, y = counts, color=name) ) 
  #geom_smooth(mapping = aes(x = MTH, y = counts, color=name) ) 
  geom_smooth(mapping = aes(x = WK, y = counts, color=name) )

ggplot(data = cleanbikecounts %>% filter(name %in% c("Hawthorne", "Tilikum") ) %>%
    mutate(DATE=as_date(date)) %>% 
    left_join(weather)) +
    geom_smooth(mapping = aes(x = TMIN, y = counts, color=name)) +
    geom_smooth(mapping = aes(x = TMAX, y = counts, color=name)) + labs(x="temperature")

#install.packages("directlabels")
plot_counts_v_weather <- ggplot(data = cleanbikecounts %>% filter(name %in% c("Hawthorne", "Tilikum") ) %>%
         mutate(DATE=as_date(date)) %>% 
         left_join(weather)) +
  geom_smooth(mapping = aes(x = TMIN, y = counts, color=name)) +
  geom_smooth(mapping = aes(x = TMAX, y = counts, color=name)) + labs(x="temperature")

BikeCountsWeather <- cleanbikecounts %>% 
  mutate(DATE=as_date(date)) %>% 
  left_join(weather) 

BikeCountsWeather %>% 
  group_by(name) %>% 
  nest()

lm(counts ~ TMIN + TMAX + PRCP, data=BikeCountsWeather) %>%  summary()

library(purrr)
library(broom)
model_df <- BikeCountsWeather %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(fit=map(data, ~lm(counts ~ TMIN + TMAX + PRCP, data=.)),
         summary=map(fit, summary),
         tidy=map(fit, tidy), 
         glance=map(fit, glance))

model_df %>% 
  select(name, glance) %>% 
  unnest(glance)


