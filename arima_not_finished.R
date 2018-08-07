library(tidyverse)
library(lubridate)
library(stringr)
library(radiant)
library(forecast)
library(astsa)
library(xts)

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


arima_hour <- encounters %>% 
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DTTM),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DTTM),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY_LEVEL_C = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3','4','5'), ACUITY_LEVEL_C, 'Others'),
                                 levels = c('Others','5','4','3','2','1'),
                                 ordered = T),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DTTM)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         BEHAVIORAL_DASHB = factor(BEHAVIORAL_DASHB),
         ADT_TIME = format(ADT_ARRIVAL_DTTM, '%Y-%m-%d %H'),
         ADT_TIME = ymd_h(ADT_TIME)) %>%
  group_by(ADT_TIME) %>% 
  summarise(n = n()) %>% 
  arrange(ADT_TIME) %>% 
  drop_na(ADT_TIME)

arima_day <- encounters %>% 
  mutate(ADT_WEEKDAYS = factor(wday(ADT_ARRIVAL_DTTM),
                               labels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
         ADT_MONTH = factor(month(ADT_ARRIVAL_DTTM),
                            labels = c('January','February','March','April','May','June','July','August','September','October','November','December')),
         ACUITY_LEVEL_C = factor(ifelse(ACUITY_LEVEL_C %in% c('1','2','3','4','5'), ACUITY_LEVEL_C, 'Others'),
                                 levels = c('Others','5','4','3','2','1'),
                                 ordered = T),
         ADT_YEAR = factor(year(ADT_ARRIVAL_DTTM)),
         ADT_HOUR = factor(hour(ADT_ARRIVAL_DTTM)),
         BEHAVIORAL_DASHB = factor(BEHAVIORAL_DASHB),
         ADT_TIME = format(ADT_ARRIVAL_DTTM, '%Y-%m-%d %H'),
         ADT_TIME = ymd_h(ADT_TIME)) %>%
  group_by(ADT_ARRIVAL_DATE) %>% 
  summarise(n = n()) %>% 
  arrange(ADT_ARRIVAL_DATE) %>% 
  drop_na(ADT_ARRIVAL_DATE)


ggsave('ts.png', ts, width = 9, height = 9)


index <- ceiling(nrow(arima_day) * 0.7)
train <- arima_day[1:index, ]
test <- arima_day[(index+1):nrow(arima_day),]

xtsObj <- xts::xts(train$n, order.by = train$ADT_ARRIVAL_DATE, frequency = 365)
plot(xtsObj)
tseries::adf.test(xtsObj)
plot(diff(xtsObj))


arima_day_log <- arima_day %>% 
  mutate(n = log(n))
train_log <- arima_day_log[1:index,]
test_log <- arima_day_log[(index+1):nrow(arima_day),]
xts_day_log <- xts(train$n, order.by = train$ADT_ARRIVAL_DATE, frequency = 365)
daygr <- diff(xts_day_log)
plot(daygr)
acf2(daygr)

acf2(xtsObj)
tsdisplay(diff(xtsObj, 7))
tsdisplay(diff(diff(xtsObj,7)))

sb <- arima(xtsObj, order = c(0,1,1), seasonal = list(order = c(3,1,3), period = 7))
summary(sb)
Box.test(resid(sb), fitdf = 7, lag = 472, type = "Ljung")
tsdisplay(resid(sb))
sb.fc <- forecast(sb, h = 472)
plot(sb.fc)
accuracy(sb.fc, test$n)


tsModel <- auto.arima(xtsObj)
summary(tsModel)


plot(forecast(tsModel, h = 472))
acf2(xtsObj)
sarima(train$n, p = 3, d = 1, q = 3)

plot(diff(arima$n))
