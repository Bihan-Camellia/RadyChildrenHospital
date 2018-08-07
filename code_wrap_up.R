# load the package
library(tidyverse)
library(lubridate)

# calculate patient volume


# read in encounters.csv and filter out all obs from "EMERGENCY" department
encounters <- read_csv("/srv/rsdata/UCSD_F_ED_ENCOUNTERS.csv") %>% 
  mutate(ADT_ARRIVAL_DTTM = dmy_hms(ADT_ARRIVAL_DTTM),
         ADT_ARRIVAL_DATE = dmy_hms(ADT_ARRIVAL_DATE)) %>% 
  filter(ED_OR_UC_DEPARTMENT == 'EMERGENCY')


# read in adtLocation.csv and filter out all obs from "EMERGENCY" department
adtLocation <- read_delim("/srv/rsdata/UCSD_ED_ADT_LOCATION.csv",
                          ";",
                          escape_double = FALSE,
                          col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                           IN_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                           OUT_DTTM = col_datetime(format = "%m/%d/%Y %H:%M")),
                          trim_ws = TRUE) %>% 
  filter(ADT_DEPARTMENT_NM_WID == 'EMERGENCY [100101010]') %>% 
  mutate(ADT_BED_LABEL_WID = gsub(' \\[.*\\]','', ADT_BED_LABEL_WID))

# use encounters to create unique id for patients went to emergency department

idList <- encounters %>% select(PAT_ENC_CSN_ID)


# read in event.csv and filter out all obs from "EMERGENCY" department

events <- read_delim("/srv/rsdata/UCSD_ED_EVENTS.csv",
                     ";",
                     escape_double = FALSE,
                     col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                      EVENT_TIME = col_datetime(format = "%m/%d/%Y %H:%M")),
                     trim_ws = TRUE) %>%
  inner_join(idList, by = 'PAT_ENC_CSN_ID')

# calculate patient volume (group to the most detailed) store as PV

PV <- encounters %>% 
  select(PAT_ENC_CSN_ID, ADT_ARRIVAL_DTTM, ACUITY_LEVEL_C, BEHAVIORAL_DASHB, ADT_ARRIVAL_DATE) %>% 
  mutate(weekdays = factor(wday(ADT_ARRIVAL_DTTM),
                           labels = c('Monday','Tuesday','Wednesday',
                                      'Thursday','Friday','Saturday','Sunday')),
         month = factor(month(ADT_ARRIVAL_DTTM),
                        labels = c('January','February','March','April','May',
                                   'June','July','August','September','October',
                                   'November','December')),
         acuity = factor(ACUITY_LEVEL_C),
         year = factor(year(ADT_ARRIVAL_DTTM)),
         hour = hour(ADT_ARRIVAL_DTTM),
         behavioral = factor(BEHAVIORAL_DASHB)) %>% 
  rename(id = PAT_ENC_CSN_ID,
         date = ADT_ARRIVAL_DATE) %>% 
  select(id, year, month, weekdays, date, hour, acuity, behavioral) %>% 
  group_by(year, month, weekdays, hour, date,acuity, behavioral) %>% 
  summarise(count = n_distinct(id)) %>% 
  ungroup()


# calculate wait time for patients store as waitTime

waitTime <- events %>% 
  inner_join(encounters, by = c('PAT_ENC_CSN_ID', 'ADT_ARRIVAL_DTTM', 'LAST_DEPARTMENT_ID')) %>% 
  select(PAT_ENC_CSN_ID, 
         EVENT_NAME, 
         EVENT_TIME,
         ADT_ARRIVAL_DTTM,
         ADT_ARRIVAL_DATE,
         ACUITY_LEVEL_C, 
         BEHAVIORAL_DASHB) %>%   
  filter(EVENT_NAME == 'Patient arrived in ED' | EVENT_NAME == 'Patient roomed in ED') %>% 
  group_by(PAT_ENC_CSN_ID, ADT_ARRIVAL_DATE, ADT_ARRIVAL_DTTM, ACUITY_LEVEL_C, BEHAVIORAL_DASHB) %>% 
  summarise(wait = as.numeric(max(EVENT_TIME)) - as.numeric(min(EVENT_TIME))) %>% 
  ungroup() %>% 
  mutate(weekdays = factor(wday(ADT_ARRIVAL_DATE),
                           labels = c('Monday','Tuesday','Wednesday',
                                      'Thursday','Friday','Saturday','Sunday')),
         month = factor(month(ADT_ARRIVAL_DATE),
                        labels = c('January','February','March','April','May',
                                   'June','July','August','September','October',
                                   'November','December')),
         acuity = factor(ACUITY_LEVEL_C),
         year = factor(year(ADT_ARRIVAL_DATE)),
         hour = hour(ADT_ARRIVAL_DTTM),
         behavioral = factor(BEHAVIORAL_DASHB)) %>% 
  select(PAT_ENC_CSN_ID, ADT_ARRIVAL_DATE, acuity, year, hour, weekdays, month, behavioral, wait) %>% 
  rename(id = PAT_ENC_CSN_ID, date = ADT_ARRIVAL_DATE)

# calculate flow time for patients store as flowTime

flowTime <- events %>% 
  filter(EVENT_NAME == 'Patient departed from ED'| EVENT_NAME == 'Patient arrived in ED' | EVENT_NAME == 'Patient roomed in ED') %>% 
  select(PAT_ENC_CSN_ID, EVENT_NAME, EVENT_TIME) %>% 
  group_by(PAT_ENC_CSN_ID, EVENT_NAME) %>% 
  mutate(ind = row_number()) %>% 
  spread(EVENT_NAME, EVENT_TIME) %>% 
  select(-ind) %>% 
  rename(arrival = `Patient arrived in ED`,
         depart = `Patient departed from ED`,
         room = `Patient roomed in ED`) %>% 
  inner_join(encounters, by = 'PAT_ENC_CSN_ID') %>% 
  ungroup() %>% 
  select(PAT_ENC_CSN_ID, 
         ADT_ARRIVAL_DTTM,
         arrival, 
         depart, 
         room,
         AGE_AT_ARRIVAL_MONTHS, 
         ACUITY_LEVEL_C, 
         MEANS_OF_ARRIVAL, 
         FIRST_CHIEF_COMPLAINT, 
         FINANCIAL_CLASS_NAME,
         LANGUAGE, PATIENT_SEX,
         BEHAVIORAL_DASHB) %>% 
  mutate(arrival = as_datetime(ifelse(is.na(arrival) & !is.na(ADT_ARRIVAL_DTTM), ADT_ARRIVAL_DTTM, arrival))) %>% 
  mutate(weekdays = factor(wday(ADT_ARRIVAL_DTTM),
                           labels = c('Monday','Tuesday','Wednesday',
                                      'Thursday','Friday','Saturday','Sunday')),
         month = factor(month(ADT_ARRIVAL_DTTM),
                        labels = c('January','February','March','April','May',
                                   'June','July','August','September','October',
                                   'November','December')),
         acuity = factor(ACUITY_LEVEL_C),
         year = factor(year(ADT_ARRIVAL_DTTM)),
         hour = hour(ADT_ARRIVAL_DTTM),
         behavioral = factor(BEHAVIORAL_DASHB)) %>% 
  select(PAT_ENC_CSN_ID, arrival, depart, room, acuity, behavioral, FIRST_CHIEF_COMPLAINT, weekdays,
         month, year, hour) %>% 
  rename(id = PAT_ENC_CSN_ID, complaint = FIRST_CHIEF_COMPLAINT)

# calculate bedToDoc time for patients store as bedToDoc

bedToDoc <- events %>% 
  inner_join(encounters, by = c('PAT_ENC_CSN_ID', 'ADT_ARRIVAL_DTTM', 'LAST_DEPARTMENT_ID')) %>% 
  filter(EVENT_NAME == "Patient roomed in ED" | EVENT_NAME == "Assign Attending" | EVENT_NAME == 'Patient departed from ED') %>% 
  select(PAT_ENC_CSN_ID, 
         ADT_ARRIVAL_DTTM, 
         EVENT_NAME, 
         EVENT_TIME, 
         ACUITY_LEVEL_C, 
         BEHAVIORAL_DASHB, 
         FIRST_CHIEF_COMPLAINT) %>% 
  unique() %>% 
  group_by(PAT_ENC_CSN_ID,
           ADT_ARRIVAL_DTTM, 
           EVENT_NAME) %>% 
  mutate(id = row_number()) %>% 
  spread(EVENT_NAME, 
         EVENT_TIME) %>% 
  select(-id) %>% 
  drop_na() %>% 
  rename(id = PAT_ENC_CSN_ID,
         bed = `Patient roomed in ED`,
         doc = `Assign Attending`,
         acuity = ACUITY_LEVEL_C,
         behavioral = BEHAVIORAL_DASHB,
         complaint = FIRST_CHIEF_COMPLAINT,
         depart = `Patient departed from ED`) %>% 
  ungroup() %>% 
  mutate(bedToDoc = as.numeric(doc - bed),
         docToDepart = as.numeric(depart - doc),
         date = date(ADT_ARRIVAL_DTTM),
         weekday = wday(ADT_ARRIVAL_DTTM),
         month = month(ADT_ARRIVAL_DTTM),
         year = year(ADT_ARRIVAL_DTTM),
         hour = hour(ADT_ARRIVAL_DTTM),
         acuity = factor(acuity),
         behavioral = factor(behavioral, labels = c('Non-Behavioral', 'Behavioral'))) %>% 
  select(-ADT_ARRIVAL_DTTM)


# calculate patient census store as patientCensus

arriveDepart <- events %>% 
  inner_join(idList, by = 'PAT_ENC_CSN_ID') %>% 
  filter(
    EVENT_NAME == 'Patient departed from ED'| EVENT_NAME == 'Patient arrived in ED'
  ) %>% select(PAT_ENC_CSN_ID, EVENT_NAME, EVENT_TIME) %>% 
  group_by(PAT_ENC_CSN_ID, EVENT_NAME) %>% 
  mutate(ind = row_number()) %>% 
  spread(EVENT_NAME, EVENT_TIME) %>% 
  select(-ind) %>% 
  rename(arrival = `Patient arrived in ED`,
         depart = `Patient departed from ED`)

patientCensus <- data.frame(
  time = seq(from = as.POSIXct('2014-01-02 0', "%Y-%m-%d %H", tz="UTC"),
             to = as.POSIXct('2017-12-31 23', "%Y-%m-%d %H", tz="UTC"),
             by = 'hour'))

for(i in 1:nrow(patientCensus)){
  patientCensus$count[i] <- sum(arriveDepart$depart > patientCensus[i,'time'] & arriveDepart$arrival <= patientCensus[i,'time'], na.rm=TRUE)
}

patientCensus <- patientCensus %>% 
  mutate(date = date(time),
         hour = factor(hour(time)),
         year = factor(year(time)),
         month = factor(month(time),
                        labels = c('January','February','March','April','May',
                                   'June','July','August','September','October',
                                   'November','December')),
         weekday = factor(wday(time),
                          labels = c('Monday','Tuesday','Wednesday',
                                     'Thursday','Friday','Saturday','Sunday')))

# code exisiting model validation

Pts123 <- 2.15
Pts45 <- 3.00
serviceLevel <- 0.75


# validation on daily patient volume
data <- encounters %>% 
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DATE),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DATE),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3'), 'High', 'Low'),
                         levels = c('Low', 'High')),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DATE)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         SEASON = factor(ifelse(ADT_MONTH %in% c('January', 'February', 'March'), 'High', 
                                ifelse(ADT_MONTH %in% c('June', 'July', 'August'), 'Low', 'Medium'))),
         WEEKDAY = factor(ifelse(ADT_WEEKDAYS %in% c('Sunday', 'Monday'), 'Mon/Sun', 'Tue/Sat')))

PtVolume_1 <- data %>% 
  filter(ADT_ARRIVAL_DATE < as.Date('2017-1-1')) %>% 
  group_by(ADT_ARRIVAL_DATE, ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPt = n_distinct(PAT_ENC_CSN_ID)) %>% 
  ungroup() %>% 
  group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPercentile = quantile(numPt, serviceLevel)) %>% 
  ungroup()

# PtEstimate <- PtVolume %>%
#   group_by(SEASON, WEEKDAY) %>%
#   summarise(EstimateTotal = sum(numPercentile))

count_1_1 <- data %>% 
  group_by(ADT_ARRIVAL_DATE, SEASON, WEEKDAY) %>% 
  summarise(n = n_distinct(PAT_ENC_CSN_ID))
# valid <- count %>% left_join(PtEstimate, by=c("SEASON", "WEEKDAY")) %>% filter(ADT_ARRIVAL_DATE > as.Date('2016-12-31'), ADT_ARRIVAL_DATE < as.Date('2018-01-01'))
# valid %>% 
#   ggplot(aes(x=ADT_ARRIVAL_DATE))+
#   geom_histogram(aes(y=n), stat='identity', na.rm = TRUE) + 
#   geom_errorbar(aes(ymax=EstimateTotal, ymin=EstimateTotal),color='red') +
#   facet_grid(WEEKDAY~., scales = 'free_x')
# valid <- valid %>% mutate(error= n-EstimateTotal,
#                           error_sq = error^2)
# 
# d_rmse <- sqrt(mean(valid$error_sq))
# d_mae <- mean(abs(valid$error))
# 
# # validation on hourly patient volume
# 
# smoothedPtVolume <- PtVolume %>% 
#   drop_na() %>% 
#   group_by(SEASON, ACUITY, WEEKDAY) %>% 
#   mutate(group = rep(1:8, each = 3)) %>% 
#   group_by(SEASON, ACUITY, WEEKDAY, group) %>% 
#   mutate(smooth = mean(numPercentile)) %>% 
#   ungroup()

count_1_2 <- data %>% 
  group_by(ADT_ARRIVAL_DATE, SEASON, WEEKDAY, ADT_HOUR) %>% 
  summarise(n = n_distinct(PAT_ENC_CSN_ID))
# 
# valid <- count %>% left_join(smoothedPtVolume, by=c("SEASON", "WEEKDAY", 'ADT_HOUR')) %>% filter(ADT_ARRIVAL_DATE > as.Date('2016-12-31'), ADT_ARRIVAL_DATE < as.Date('2018-01-01'), !is.na(ADT_HOUR))
# 
# valid <- valid %>% mutate(error= n-smooth,
#                           error_sq = error^2,
#                           dttm = ymd_h(paste(ADT_ARRIVAL_DATE, ADT_HOUR)))
# 
# h_rmse <- sqrt(mean(valid$error_sq))
# h_mae <- mean(abs(valid$error))


# # model recreation part store as PVRec
PVRec <- data %>% 
  group_by(ADT_ARRIVAL_DATE, ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPt = n()) %>% 
  ungroup() %>% 
  group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPercentile = quantile(numPt, serviceLevel)) %>% 
  ungroup()

# new grouping

data <- encounters %>% 
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DATE),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DATE),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3'), 'High', 'Low'),
                         levels = c('Low', 'High')),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DATE)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         SEASON = factor(ifelse(ADT_MONTH %in% c('January', 'February', 'March'), 'High', 
                                ifelse(ADT_MONTH %in% c('June', 'July', 'August'), 'Low', 'Medium'))),
         WEEKDAY = factor(ifelse(ADT_WEEKDAYS %in% c('Sunday', 'Monday', 'Tuesday'), 'Sun/Tue', 'Wed/Sat')))

PtVolume_2 <- data %>% 
  filter(ADT_ARRIVAL_DATE < as.Date('2017-1-1')) %>% 
  group_by(ADT_ARRIVAL_DATE, ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPt = n_distinct(PAT_ENC_CSN_ID)) %>% 
  ungroup() %>% 
  group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPercentile = quantile(numPt, serviceLevel)) %>% 
  ungroup()

# PtEstimate <- PtVolume %>%
#   group_by(SEASON, WEEKDAY, ACUITY) %>%
#   summarise(EstimateTotal = sum(numPercentile))

count_2_1 <- data %>% 
  group_by(ADT_ARRIVAL_DATE, SEASON, WEEKDAY, ACUITY) %>% 
  summarise(n = n_distinct(PAT_ENC_CSN_ID))

# valid <- count %>% left_join(PtEstimate, by=c("SEASON", "WEEKDAY", 'ACUITY')) %>% filter(ADT_ARRIVAL_DATE > as.Date('2016-12-31'), ADT_ARRIVAL_DATE < as.Date('2018-01-01'))
# 

# valid %>% 
#   ggplot(aes(x=ADT_ARRIVAL_DATE))+
#   geom_histogram(aes(y=n, fill = ACUITY), stat='identity', na.rm = TRUE, position = 'dodge') + 
#   geom_errorbar(aes(ymax=EstimateTotal, ymin=EstimateTotal, color=ACUITY)) +
#   facet_grid(WEEKDAY~ACUITY, scales = 'free_x')

# valid <- valid %>% mutate(error= n-EstimateTotal,
#                           error_sq = error^2)
# 
# d_rmse <- sqrt(mean(valid$error_sq))
# 
# d_mae <- mean(abs(valid$error))

# smoothedPtVolume <- PtVolume %>% 
#   drop_na() %>% 
#   group_by(SEASON, ACUITY, WEEKDAY) %>% 
#   mutate(group = rep(1:8, each = 3)) %>% 
#   group_by(SEASON, ACUITY, WEEKDAY, group) %>% 
#   mutate(smooth = mean(numPercentile)) %>% 
#   ungroup()

count_2_2 <- data %>% 
  group_by(ADT_ARRIVAL_DATE, SEASON, WEEKDAY, ADT_HOUR) %>% 
  summarise(n = n())

# valid <- count %>% left_join(smoothedPtVolume, by=c("SEASON", "WEEKDAY", 'ADT_HOUR')) %>% filter(ADT_ARRIVAL_DATE > as.Date('2016-12-31'), ADT_ARRIVAL_DATE < as.Date('2018-01-01'), !is.na(ADT_HOUR))
# 
# valid <- valid %>% mutate(error= n-smooth,
#                           error_sq = error^2,
#                           dttm = ymd_h(paste(ADT_ARRIVAL_DATE, ADT_HOUR)))
# 
# h_rmse <- sqrt(mean(valid$error_sq))
# 
# h_mae <- mean(abs(valid$error))


# # model recreation function
calcStaff <- function(df){
  smooth <- df$smooth
  smooth <- c(smooth[23:24], smooth)
  staff <- rep(NA, 26)
  
  for(i in 1:length(smooth)){
    staff[i] <-max(smooth[i], smooth[i+1], smooth[i+2])
  }
  return(staff[1:24])
}
