library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)      
library(fpp2)           
library(zoo)
library(radiant)
library(radiant.basics)

time_stay <- readRDS("time_stay.rds") %>% 
  mutate(arrival_hour= hour(arrival),
         stay = stay+1,
         leave_hour = hour(depart)) %>% 
  drop_na()

time_stay$stay <- ifelse(time_stay$stay<0, time_stay$stay+24, time_stay$stay)

time_stay$year <- year(time_stay$arrival)

hour_name <- paste0("clock",0:23)

time_stay[hour_name] <- 0


# for (i in 1:length(time_stay$PAT_ENC_CSN_ID)) {
#   for (j in ((time_stay$arrival_hour[i]):(time_stay$leave_hour[i]))) {
#     time_stay[paste0('clock',j)] <- 1
#   }
# }

time_stay_by_arrival_time <- time_stay %>% 
  group_by(year,arrival_hour,ACUITY_LEVEL_C) %>% 
  summarise(stay_time = mean(stay),
            patient_number=n()) 

# standard deviation 0.1
sd(time_stay_by_arrival_time$stay_time)

#round to 4 hours per person


rollsum(c(3:30), k=4)


time_stay_by_arrival_time$year <- as.factor(time_stay_by_arrival_time$year)

# use rollsum() for predicted value, k = 4,
# eg., patient_population = rollsum(predicted_hourly_patient_arrival_volume, k=4)

visualize(
  time_stay_by_arrival_time, 
  xvar = "arrival_hour", 
  yvar = "stay_time", 
  type = "scatter", 
  nrobs = -1, 
  facet_row = "ACUITY_LEVEL_C", 
  color = "year", 
  size = "patient_number", 
  alpha = 0.71, 
  custom = FALSE
)

