library(tidyverse)
library(lubridate)
library(stringr)
library(radiant)


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

# events <- read_delim("/srv/rsdata/UCSD_ED_EVENTS.csv",
#                      ";",
#                      escape_double = FALSE,
#                      col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
#                                       EVENT_TIME = col_datetime(format = "%m/%d/%Y %H:%M")),
#                      trim_ws = TRUE)
# 
# adtLocation <- read_delim("/srv/rsdata/UCSD_ED_ADT_LOCATION.csv",
#                           ";",
#                           escape_double = FALSE,
#                           col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
#                                            IN_DTTM = col_datetime(format = "%m/%d/%Y %H:%M"),
#                                            OUT_DTTM = col_datetime(format = "%m/%d/%Y %H:%M")),
#                           trim_ws = TRUE)
#
# diagnosis <- read_delim("/srv/rsdata/UCSD_ED_ENCOUNTER_DIAG.csv",
#                         ";",
#                         escape_double = FALSE, col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M")),
#                         trim_ws = TRUE)
#
# chiefComplaint <- read_delim("/srv/rsdata/UCSD_ED_CHIEF_COMPLAINT.csv",
#                              ";",
#                              escape_double = FALSE,
#                              col_types = cols(ADT_ARRIVAL_DTTM = col_datetime(format = "%m/%d/%Y %H:%M")),
#                              trim_ws = TRUE)



# Look up number of unique patients

# n_distinct(encounters$PAT_ENC_CSN_ID) # 638273
# n_distinct(encounters$PAT_ID)
# n_distinct(events$PAT_ENC_CSN_ID) # 638273
# n_distinct(adtLocation$PAT_ENC_CSN_ID) # 638186
# n_distinct(chiefComplaint$PAT_ENC_CSN_ID) # 630258
# n_distinct(diagnosis$PAT_ENC_CSN_ID) # 630036

# `encounters` and `events` have same amount of unique patients

# create new dataframe from `encounters` with new variables: ADT_WEEKDAYS, ADT_MONTH

# first study how days of the week has an impact on the number of patients:

avgPt <- encounters %>%
  select(PAT_ENC_CSN_ID,
         AGE_AT_ARRIVAL_MONTHS,
         ADT_ARRIVAL_DTTM,
         ACUITY_LEVEL_C,
         ADT_ARRIVAL_DATE,
         BEHAVIORAL_DASHB) %>%
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DTTM),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DTTM),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY_LEVEL_C = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3','4','5'), ACUITY_LEVEL_C, 'Others'),
                                 levels = c('Others','5','4','3','2','1'),
                                 ordered = T),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DTTM)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         BEHAVIORAL_DASHB = factor(BEHAVIORAL_DASHB))
saveRDS(avgPt, 'avgPt.rds')

avgPt <- readRDS('avgPt.rds')


meanComparison <- avgPt %>% 
  drop_na(ADT_WEEKDAYS) %>% 
  group_by(ADT_YEAR, ADT_MONTH, ADT_ARRIVAL_DATE,ADT_WEEKDAYS) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(ADT_YEAR, ADT_MONTH, ADT_WEEKDAYS) %>% 
  summarise(avg = mean(n)) %>% 
  ungroup()

hist <- avgPt %>% 
  drop_na(ADT_WEEKDAYS) %>% 
  group_by(ADT_YEAR, ADT_MONTH, ADT_ARRIVAL_DATE,ADT_WEEKDAYS) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(ADT_YEAR, ADT_WEEKDAYS) %>% 
  summarise(avg = mean(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = avg)) +
  geom_histogram(binwidth = 15) +
  ggtitle('')
ggsave('PtDist.png',hist)


avgPt_wday <- avgPt %>% 
  drop_na(ADT_WEEKDAYS) %>% 
  group_by(ADT_YEAR, ADT_MONTH, ADT_ARRIVAL_DATE,ADT_WEEKDAYS) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(ADT_YEAR, ADT_WEEKDAYS) %>% 
  summarise(avg = mean(n)) %>% 
  ggplot(aes(x = ADT_WEEKDAYS, y = avg, fill = ADT_YEAR)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = ADT_WEEKDAYS, y = avg, label = round(avg, 2)), size = 3, vjust = 1.5) +
  ggtitle('Average Patient Volume by Weekday, 2014 - 2018') +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  facet_grid(ADT_YEAR~., scales = 'free_y') +
  scale_fill_brewer(palette = 'Pastel1')
# avgPt_wday
ggsave('avgPtbyWday.png', avgPt_wday, width = 7, height = 7)

result <- compare_means(
  meanComparison, 
  var1 = "ADT_WEEKDAYS", 
  var2 = "avg", 
  samples = "paired", 
  adjust = "bonf"
)
summary(result, show = TRUE)
plot(result, plots = "scatter", custom = FALSE)


avgPt_m <- avgPt %>% 
  drop_na(ADT_MONTH) %>% 
  group_by(ADT_MONTH,ADT_ARRIVAL_DATE, ADT_YEAR) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(ADT_MONTH, ADT_YEAR) %>% 
  summarise(avg = mean(n)) %>% 
  # mutate(avg_bin = cut(avg, breaks = seq(325, 520, 22), ordered_result = TRUE)) %>% 
  ggplot(aes(x = ADT_MONTH, y = avg, fill = ADT_YEAR)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = ADT_MONTH, y = avg, label = round(avg, 2)), size = 2, vjust = 1.5) +
  ggtitle('Average Patient Volume by Month, 2014 - 2018') +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  facet_grid(ADT_YEAR~., scales = 'free_x') +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(axis.text.x = element_text(size = 6))
# avgPT_m
ggsave('avgPtbyMonth.png', avgPt_m, width = 7, height = 7)

result <- compare_means(
  meanComparison, 
  var1 = "ADT_MONTH", 
  var2 = "avg", 
  samples = "paired", 
  adjust = "bonf"
)
summary(result, show = TRUE)
plot(result, plots = "scatter", custom = FALSE)


avgPt_acuity<- avgPt %>% 
  drop_na(ADT_YEAR) %>% 
  group_by(ADT_MONTH, ADT_ARRIVAL_DATE, ADT_YEAR, ADT_WEEKDAYS, ACUITY_LEVEL_C) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(ACUITY_LEVEL_C, ADT_YEAR, ADT_WEEKDAYS) %>% 
  summarise(avg= mean(n)) %>% 
  ggplot(aes(x = ADT_WEEKDAYS, y = avg, fill = ACUITY_LEVEL_C)) +
  geom_col(position = 'dodge') +
  ggtitle('Average Patient Volume by Acuities, 2014 - 2018') +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  facet_grid(ADT_YEAR~., scales = 'free') +
  scale_fill_brewer(palette = 'OrRd') +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        # strip.text = element_text(size = 4),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.5,'cm'),
        title = element_text(hjust = 0.5)) +
  labs(x = NULL, y = NULL)
# avgPt_acuity
ggsave('avgPtbyAcuity.png', avgPt_acuity, width = 7, height = 7)
