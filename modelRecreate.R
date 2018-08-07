library(tidyverse)
library(lubridate)

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


Pts123 <- 2.15
Pts45 <- 3.00
serviceLevel <- 0.75

### Scheduling Part

highAcSchedule <- data_frame(Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 
                                      'Orange', 'Orange', 'Orange', 'Float'),
                             TimeSlot = c('6a-3p', '7a-4p', '2p-11p', 
                                          '3p-12a', '10p-6a', '11p-7a', 
                                          '6a-12p', '12p-7p', '7p-2a', '9a-3p'),
                             H0 = c(0,0,0,0,1,1,0,0,1,0),
                             H1 = c(0,0,0,0,1,1,0,0,1,0),
                             H2 = c(0,0,0,0,1,1,0,0,0,0),
                             H3 = c(0,0,0,0,1,1,0,0,0,0),
                             H4 = c(0,0,0,0,1,1,0,0,0,0),
                             H5 = c(0,0,0,0,1,1,0,0,0,0),
                             H6 = c(1,0,0,0,0,1,1,0,0,0),
                             H7 = c(1,1,0,0,0,0,1,0,0,0),
                             H8 = c(1,1,0,0,0,0,1,0,0,0),
                             H9 = c(1,1,0,0,0,0,1,0,0,1),
                             H10 = c(1,1,0,0,0,0,1,0,0,1),
                             H11 = c(1,1,0,0,0,0,1,0,0,1),
                             H12 = c(1,1,0,0,0,0,0,1,0,1),
                             H13 = c(1,1,0,0,0,0,0,1,0,1),
                             H14 = c(0,1,1,0,0,0,0,1,0,1),
                             H15 = c(0,0,1,1,0,0,0,1,0,1),
                             H16 = c(0,0,1,1,0,0,0,1,0,0),
                             H17 = c(0,0,1,1,0,0,0,1,0,0),
                             H18 = c(0,0,1,1,0,0,0,1,0,0),
                             H19 = c(0,0,1,1,0,0,0,0,1,0),
                             H20 = c(0,0,1,1,0,0,0,0,1,0),
                             H21 = c(0,0,1,1,0,0,0,0,1,0),
                             H22 = c(0,0,0,1,1,0,0,0,1,0),
                             H23 = c(0,0,0,0,1,1,0,0,1,0))


lowAcSchedule <- data_frame(Zone = c('FT am', 'FT pm', 'FT ON', 'P', 
                                     'P', 'EDS 1', 'EDS2'),
                            TimeSlow = c('9a-4p', '4p-10p', '9:30p-4a', 
                                         '10a-6p', '6p-2a', 
                                         '5:30p-1:30a(weekdays)','5p-12p'),
                            H0 = c(0,0,1,0,1,1,0),
                            H1 = c(0,0,1,0,1,.5,0),
                            H2 = c(0,0,1,0,0,0,0),
                            H3 = c(0,0,1,0,0,0,0),
                            H4 = rep(0,7),
                            H5 = rep(0,7),
                            H6 = rep(0,7),
                            H7 = rep(0,7),
                            H8 = rep(0,7),
                            H9 = c(1,0,0,0,0,0,0),
                            H10 = c(1,0,0,0,0,0,0),
                            H11 = c(1,0,0,0,0,0,0),
                            H12 = c(1,0,0,1,0,0,0),
                            H13 = c(1,0,0,1,0,0,0),
                            H14 = c(1,0,0,1,0,0,0),
                            H15 = c(1,0,0,1,0,0,0),
                            H16 = c(0,1,0,1,0,0,0),
                            H17 = c(0,1,0,1,0,.5,1),
                            H18 = c(0,1,0,0,1,1,1),
                            H19 = c(0,1,0,0,1,1,1),
                            H20 = c(0,1,0,0,1,1,1),
                            H21 = c(0,1,.5,0,1,1,1),
                            H22 = c(0,0,1,0,1,1,1),
                            H23 = c(0,0,1,0,1,1,1))


### Calculate 75th Percentile hourly patient volume

data <- encounters %>% 
  filter(ACUITY_LEVEL_C != 'null') %>% # when this filter applies, the number seems to match the spreadsheet model;
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DTTM),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DTTM),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3'), 'High', 'Low'),
                         levels = c('Low', 'High')),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DTTM)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         SEASON = factor(ifelse(ADT_MONTH %in% c('January', 'February'), 'High', 
                                ifelse(ADT_MONTH %in% c('June', 'July', 'August'), 'Low', 'Medium'))),
         WEEKDAY = factor(ifelse(ADT_WEEKDAYS %in% c('Sunday', 'Monday'), 'Mon/Sun', 'Tue/Sat')))

# Make a plot

PtCount <- data %>% 
  group_by(ADT_ARRIVAL_DATE, ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPt = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ADT_HOUR)) %>% 
  ggplot(aes(x = ADT_HOUR, y = numPt, color = ACUITY)) +
  geom_boxplot() +
  facet_grid(SEASON~WEEKDAY) +
  labs(x = NULL, y = NULL) +
  ggtitle('Distribution of Patients count') +
  theme_bw() +
  scale_color_brewer(palette = 'Dark2') +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 2),
        legend.position = 'bottom',
        legend.title = element_text(size = 8),
        axis.ticks = element_line())

# ggsave('PtCount.png', PtCount, width = 16, height = 9)


PtVolume <- data %>% 
  group_by(ADT_ARRIVAL_DATE, ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPt = n()) %>% 
  ungroup() %>% 
  group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY) %>% 
  summarise(numPercentile = quantile(numPt, serviceLevel)) %>% 
  ungroup()

# Recreate 75th Percentile Plot
## Mon/Sun
PtVolume %>% 
  filter(SEASON == 'High',
         # ACUITY == 'High',
         WEEKDAY == 'Mon/Sun') %>% 
  ggplot(aes(x = ADT_HOUR, y = numPercentile, fill = ACUITY)) +
  geom_col(position = 'dodge') +
  theme_minimal() + 
  ggtitle('ED Arrivals 75th Percentile - Sun-Mon - High Demand Season') +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 8)) +
  scale_fill_brewer(palette = 'Set2') +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15,3))

#Tue/Sat
PtVolume %>% 
  filter(SEASON == 'High',
         # ACUITY == 'High',
         WEEKDAY == 'Tue/Sat') %>% 
  ggplot(aes(x = ADT_HOUR, y = numPercentile, fill = ACUITY)) +
  geom_col(position = 'dodge') +
  theme_minimal() + 
  ggtitle('ED Arrivals 75th Percentile - Tue-Sat - High Demand Season') +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 8)) +
  scale_fill_brewer(palette = 'Set2') +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15,3))


# staff care need calculation
calcStaff <- function(df){
  smooth <- df$smooth
  smooth <- c(smooth[23:24], smooth)
  staff <- rep(NA, 26)
  
  for(i in 1:length(smooth)){
    staff[i] <-max(smooth[i], smooth[i+1], smooth[i+2])
  }
  return(staff[1:24])
}

datPassH <- PtVolume %>% 
  filter(SEASON == 'High',
         ACUITY == 'High',
         WEEKDAY == 'Mon/Sun') %>% 
  select(ADT_HOUR, numPercentile) %>% 
  mutate(group = rep(1:8, each = 3)) %>% 
  group_by(group) %>% 
  mutate(smooth = mean(numPercentile)) %>% 
  ungroup() %>% 
  select(ADT_HOUR, smooth) 
datPassH <- datPassH %>% 
  mutate(staffingNeed = calcStaff(datPassH) / Pts123)

datPassL <- PtVolume %>% 
  filter(SEASON == 'High',
         ACUITY == 'Low',
         WEEKDAY == 'Mon/Sun') %>% 
  select(ADT_HOUR, numPercentile) %>% 
  mutate(group = rep(1:8, each = 3)) %>% 
  group_by(group) %>% 
  mutate(smooth = mean(numPercentile)) %>% 
  ungroup() %>% 
  select(ADT_HOUR, smooth)
datPassL <- datPassL %>% 
  mutate(staffingNeed = calcStaff(datPassL) / Pts45)


df <- data_frame(hour = factor(seq(0,23,1)), 
                 patientEstimateH = datPassH$smooth,
                 staffingNeedH = datPassH$staffingNeed,
                 staffingPlanH = colSums(highAcSchedule[-1:-2], na.rm = TRUE),
                 patientEstimateL = datPassL$smooth,
                 staffingNeedL = datPassL$staffingNeed,
                 staffingPlanL = colSums(lowAcSchedule[-1:-2], na.rm = TRUE),
                 staffingDiffH = - staffingNeedH + staffingPlanH,
                 staffingDiffL = - staffingNeedL + staffingPlanL,
                 totalPtEst = patientEstimateH + patientEstimateL,
                 totalStaffingNeed = staffingNeedH + staffingNeedL,
                 totalStaffingPlan = staffingPlanH + staffingPlanL,
                 totalStaffingDiff = - totalStaffingNeed + totalStaffingPlan)

# Recreate Staffing planned vs. Staffing Est Plot
# High Acuities
df %>% ggplot(aes(x = hour)) +
  geom_col(aes(y = staffingPlanH), fill = 'blue', alpha = 0.5) +
  geom_errorbar(aes(ymax = staffingNeedH, ymin = staffingNeedH)) +
  geom_text(aes(y = staffingNeedH, label = round(staffingNeedH, 2)), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_brewer(palette = 'Set2') +
  scale_color_brewer(palette = 'Set2') +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,2))
# Low Acuities
df %>% ggplot(aes(x = hour)) +
  geom_col(aes(y = staffingPlanL), fill = 'blue', alpha = 0.5) +
  geom_errorbar(aes(ymax = staffingNeedL, ymin = staffingNeedL)) +
  geom_text(aes(y = staffingNeedL, label = round(staffingNeedL, 2)), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_brewer(palette = 'Set2') +
  scale_color_brewer(palette = 'Set2') +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,2))
# Total
df %>% ggplot(aes(x = hour)) +
  geom_col(aes(y = totalStaffingPlan)) +
  geom_errorbar(aes(ymax = totalStaffingNeed, ymin = totalStaffingNeed)) +
  geom_text(aes(y = totalStaffingNeed, label = round(totalStaffingNeed, 2)), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_brewer(palette = 'Set2') +
  scale_color_brewer(palette = 'Set2') +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,2))

