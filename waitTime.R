library(tidyverse)
library(lubridate)
library(stringr)



encounters <- read_csv("/srv/rsdata/UCSD_F_ED_ENCOUNTERS.csv",
                       col_types = cols(ADT_ARRIVAL_DATE = col_date(format = "%m/%d/%Y"),
                                        ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        ED_DEPARTURE_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        ED_DISPOSITION_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        EMERGENCY_ADMISSION_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        HOSPITAL_ADMISSION_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        HOSPITAL_DISCHARGE_DATE = col_date(format = "%m/%d/%Y"),
                                        HOSPITAL_DISCHARGE_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        UPDATE_DATE = col_datetime(format = "%m/%d/%Y %H:%M")))

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

adtLocation <- read_delim("/srv/rsdata/UCSD_ED_ADT_LOCATION.csv",
                          ";",
                          escape_double = FALSE,
                          col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                           IN_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
                                           OUT_DTTM = col_datetime(format = "%m/%d/%Y %H:%M")),
                          trim_ws = TRUE)

wTime <- adtLocation %>% 
  left_join(encounters, by = c('PAT_ENC_CSN_ID', 'ADT_ARRIVAL_DTTM', 'LAST_DEPARTMENT_ID')) %>% 
  select(PAT_ENC_CSN_ID, 
         ADT_ARRIVAL_DTTM, 
         IN_DTTM,
         OUT_DTTM,
         BEHAVIORAL_DASHB,
         AGE_AT_ARRIVAL_MONTHS,
         ACUITY_LEVEL_C,
         FIRST_CHIEF_COMPLAINT) %>% 
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DTTM),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DTTM),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY_LEVEL_C = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3','4','5'), ACUITY_LEVEL_C, 'Others'),
                                 levels = c('Others','5','4','3','2','1'),
                                 ordered = T),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DTTM)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         BEHAVIORAL_DASHB = factor(BEHAVIORAL_DASHB)) %>% 
  group_by(PAT_ENC_CSN_ID, ADT_MONTH, ADT_WEEKDAYS, ACUITY_LEVEL_C, ADT_YEAR, BEHAVIORAL_DASHB) %>% 
  mutate(wait = min(IN_DTTM) - ADT_ARRIVAL_DTTM) %>% 
  summarise(wait = mean(wait)) %>% 
  ungroup() %>% 
  mutate(wait = as.numeric(wait) / 60)

# avgWait_wday <- wTime %>% 
#   group_by(ADT_YEAR, ADT_MONTH, ADT_WEEKDAYS) %>% 
#   summarise(avg = mean(wait)) %>% 
#   ggplot(aes(x = ADT_WEEKDAYS, y = avg, color = ADT_YEAR)) +
#   geom_point(position = position_jitter(0.2))+
#   geom_line(aes(group = ADT_YEAR), position = position_jitter(0.2), linetype = 2, alpha = 0.2) +
#   facet_grid(ADT_MONTH~.) +
#   theme_bw() +
#   scale_fill_brewer(palette = 'Dark2') +
#   ggtitle('Average Wait Time 2014 - 2018') +
#   theme(axis.text.x = element_text(size = 5),
#         axis.text.y = element_text(size = 5),
#         strip.text = element_text(size = 4),
#         legend.position = 'bottom',
#         legend.title = element_blank(),
#         legend.text = element_text(size = 5),
#         legend.key.size = unit(0.5,'cm')) +
#   labs(x = NULL, y = NULL)
# 
# ggsave('a.png', avgWait_wday)

avgWait_wday <- wTime %>% 
  group_by(ADT_YEAR, ADT_MONTH, ADT_WEEKDAYS) %>% 
  summarise(avg = mean(wait)) %>% 
  ggplot(aes(x = ADT_WEEKDAYS, y = avg, fill = ADT_YEAR)) +
  geom_col(position = 'dodge') +
  # geom_text(aes(x = ADT_WEEKDAYS, y = avg, label = round(avg, 2)), size = 3, vjust = 1.5) +
  facet_grid(ADT_MONTH~.) +
  theme_bw() +
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Average Wait Time 2014 - 2018') +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        strip.text = element_text(size = 4),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.5,'cm')) +
  labs(x = NULL, y = NULL)

# ggsave('avgWTbyMonth.png',avgWait_wday)

