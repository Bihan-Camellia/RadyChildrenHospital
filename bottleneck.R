library(tidyverse)
library(plyr)
library(zoo)

adtLocation <- read_delim("/srv/rsdata/UCSD_ED_ADT_LOCATION.csv",
                          ";",
                          escape_double = FALSE,
                          col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                           IN_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                           OUT_DTTM = col_datetime(format = "%m/%d/%Y %H:%M")),
                          trim_ws = TRUE)

adtLocation <- adtLocation %>% 
  filter(ADT_DEPARTMENT_NM_WID == 'EMERGENCY [100101010]') %>% 
  mutate(ADT_BED_LABEL_WID = gsub(' \\[.*\\]','', ADT_BED_LABEL_WID)) %>% 
  filter(!grepl('-WR', ADT_BED_LABEL_WID),
         !grepl('-EDX-', ADT_BED_LABEL_WID),
         !grepl('-TRG', ADT_BED_LABEL_WID),
         !grepl('-RMA', ADT_BED_LABEL_WID),
         !ADT_BED_LABEL_WID %in% c('zCHECKOUT', 'Flu Zone', 'CHARGER', 'EDS INTERNAL','SIM')) 

events <- read_delim("/srv/rsdata/UCSD_ED_EVENTS.csv",
                     ";",
                     escape_double = FALSE,
                     col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                      EVENT_TIME = col_datetime(format = "%m/%d/%Y %H:%M")),
                     trim_ws = TRUE)

encounters <- read_csv("/srv/rsdata/UCSD_F_ED_ENCOUNTERS.csv") %>% 
  mutate(ADT_ARRIVAL_DTTM = dmy_hms(ADT_ARRIVAL_DTTM),
         ADT_ARRIVAL_DATE = dmy_hms(ADT_ARRIVAL_DATE)) %>% 
  filter(ED_OR_UC_DEPARTMENT == 'EMERGENCY')

encounters <- bind_rows(
  encounters %>% filter(ED_OR_UC_DEPARTMENT == 'EMERGENCY'),
  encounters %>% filter(ED_OR_UC_DEPARTMENT == 'Kiswahili') %>% 
    mutate(LANGUAGE = paste(LANGUAGE, ED_OR_UC_DEPARTMENT, sep = '/'),
           ED_OR_UC_DEPARTMENT = BEHAVIORAL_DASHB,
           BEHAVIORAL_DASHB = as.integer(X77)),
  encounters %>% filter(ED_OR_UC_DEPARTMENT == 'English') %>% 
    mutate(FINANCIAL_CLASS_NAME = PATIENT_SEX,
           PATIENT_SEX = LANGUAGE,
           LANGUAGE = ED_OR_UC_DEPARTMENT,
           ED_OR_UC_DEPARTMENT = BEHAVIORAL_DASHB,
           BEHAVIORAL_DASHB = as.integer(X77))
) %>% 
  select(-X77)
useful <- encounters %>% select(PAT_ENC_CSN_ID, ACUITY_LEVEL_C, BEHAVIORAL_DASHB, FIRST_CHIEF_COMPLAINT)

filtered <- events %>% 
  inner_join(useful, by = 'PAT_ENC_CSN_ID')

# View(adtLocation%>% 
#        filter(ADT_BED_LABEL_WID %in% c('17-FLU','17-O')) %>%
#        select(ADT_ARRIVAL_DTTM,ADT_ROOM_ID, ADT_BED_LABEL_WID) %>% 
#        group_by(ADT_BED_LABEL_WID) %>% 
#        slice(1:10))

adtLocation <- adtLocation %>% 
  mutate(ADT_BED_LABEL_WID = plyr::mapvalues(ADT_BED_LABEL_WID, from = c('12-FLU', '13-FLU','14-FLU','15-FLU','16-FLU','17-FLU','18-FLU','19-FLU','20-FLU','21-FLU','A- Y','01-Tr','02-Tr'),
                                             to = c('12-B','13-B','14-B','15-B','16-B','17-O','18-O','19-O','20-O','21-O','A-Y','01-Y','02-Y'))) %>% 
  select(PAT_ENC_CSN_ID, IN_DTTM, OUT_DTTM, ADT_BED_LABEL_WID, ADT_ARRIVAL_DTTM)
location <- adtLocation %>% 
  gather(key =type, value = EVENT_TIME, -PAT_ENC_CSN_ID, -ADT_BED_LABEL_WID, -ADT_ARRIVAL_DTTM)

total <- filtered %>% left_join(location, by = c('PAT_ENC_CSN_ID', "ADT_ARRIVAL_DTTM", "EVENT_TIME"))

total_small <- total %>% select(PAT_ENC_CSN_ID, EVENT_TIME, type, ADT_BED_LABEL_WID) %>% drop_na() %>% unique()

total_small <- total_small %>% mutate(type = plyr::mapvalues(type, from = c("IN_DTTM", "OUT_DTTM"), to = c('in','out')), label = paste(ADT_BED_LABEL_WID, type, sep = '_'))
total_small <- total_small %>% mutate(date = date(EVENT_TIME), hour = hour(EVENT_TIME)) %>% arrange(date, hour)
total_new <- total_small %>% 
  dplyr::rename(bed = ADT_BED_LABEL_WID) %>% 
  select(date, hour, bed, type) %>% 
  dplyr::group_by(date, hour) %>% 
  dplyr::mutate(id = row_number()) %>% 
  spread(bed, type)
total_new <- total_new %>% replace(na.locf(total_new) == 'in', 'in')
total_new <- total_new %>% 
  select(-id) %>% 
  unique()
repl <- total_new[,3:65]
repl[repl == 'in'] <- 1
repl[repl == 'out'] <- 0
repl <- as.data.frame(sapply(repl, as.numeric))
total_new[,3:65] <- repl

head(total_new)

total_new$rowSum <- rowSums(total_new[3:65],na.rm=TRUE)

total_final <- total_new %>% select(date, hour, rowSum) %>% 
  group_by(date, hour) %>% 
  dplyr::summarise(numBeds = max(rowSum)) %>% 
  ungroup()


filter_date <- seq(from = as.Date('2015-08-16'),
                   to = as.Date('2015-08-23'),
                   by = 'day')
f <- filtered %>% 
  filter(PROV_TYPE == 'Physician') %>% 
  mutate(date = date(EVENT_TIME),
         hour = hour(EVENT_TIME)) %>% 
  mutate(in_date = date %in% filter_date,
         event = EVENT_NAME == 'Charting Complete',
         true = in_date&event) %>% 
  group_by(date,hour, PROV_NAME) %>% 
  mutate(n = n()) %>% 
  filter(!true, n > 3) %>%
  ungroup() %>% 
  group_by(date, PROV_NAME) %>% 
  summarise(first = min(EVENT_TIME),
            last = max(EVENT_TIME))

phy_census <- data.frame(
  time = seq(from = ymd_h('2014-01-02 0'),
             to = ymd_h('2017-12-31 23'),
             by = 'hour'))

for(i in 1:nrow(phy_census)){
  phy_census$count[i] <- sum(f$last > phy_census[i,'time'] & f$first <= phy_census[i,'time'], na.rm=TRUE)
}

# write_csv(phy_census, 'phy_census.csv')  

hist(phy_census$count)
phy_census <- phy_census %>% mutate(date = date(time), hour = hour(time))


patient <- filtered %>% 
  filter(EVENT_NAME == 'Patient departed from ED') %>% 
  mutate(date = date(EVENT_TIME),
         hour = hour(EVENT_TIME)) %>% 
  group_by(date, hour) %>% 
  dplyr::summarise(patient = n())

summary <- total_final %>% 
  select(date, hour, numBeds) %>% 
  inner_join(phy_census, by = c('date','hour')) %>% 
  inner_join(patient, by = c('date','hour')) %>%
  select(-time) %>%
  rename(physician = count)

summary <- summary %>% 
  mutate(month = month(date),
         weekday = wday(date)) %>% 
  mutate(season = ifelse(month %in% c(1,2,3), "Winter", ifelse(month %in% c(6,7,8), 'Summer', 'Spring/Fall')))

total_final %>% filter(date == as.Date('2017-06-13'))


df <- data.frame(hour = rep(seq(0,23,1),3),
                 season = c(rep('Summer',24), rep("Spring/Fall", 24), rep('Winter', 24)),
                 beds = c(rep(19,7), rep(15,2),rep(23,5), rep(27,10),
                          rep(38,2), rep(26,2),rep(19,3),rep(27, 2),rep(30,3),rep(38,12),
                          rep(51,2), rep(34,2),rep(26,3),rep(34,2),rep(51,7), rep(59, 8)))

hmm <- summary %>% left_join(df, by = c('season','hour')) %>% 
  mutate(utilization = numBeds / beds)

View(hmm %>% filter(utilization > 1))


# write_csv(summary, 'summary.csv')

output <- summary %>% filter(hour %in% c(18, 19, 20, 21, 22,23, 0)) %>% group_by(physician) %>% summarise(avg = mean(patient), n = n())
output %>% ggplot(aes(x = physician, y = avg, size = n)) + geom_point()+geom_smooth()
output %>% ggplot(aes(x = physician, y = avg)) + geom_col()
