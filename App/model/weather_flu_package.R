# install.packages("cdcfluview")
library(cdcfluview)
library(dplyr)


dat <- cdcfluview::get_flu_data(region = 'hhs',
                    sub_region = 1:10,
                    data_source = 'ilinet',
                    years = 2000:2014)
# dat %>%
#   mutate(REGION=factor(REGION,
#                        levels=unique(REGION),
#                        labels=c("Boston", "New York",
#                                 "Philadelphia", "Atlanta",
#                                 "Chicago", "Dallas",
#                                 "Kansas City", "Denver",
#                                 "San Francisco", "Seattle"),
#                        ordered=TRUE)) %>%
#   mutate(season_week=ifelse(WEEK>=40, WEEK-40, WEEK),
#          season=ifelse(WEEK<40,
#                        sprintf("%d-%d", YEAR-1, YEAR),
#                        sprintf("%d-%d", YEAR, YEAR+1)))


cal <- ili_weekly_activity_indicators(years = c(2013:2018)) %>% 
  dplyr::filter(statename=='California') %>% 
  arrange(weekend)

ny <- ili_weekly_activity_indicators(years = c(2013:2018)) %>% 
  dplyr::filter(statename=='New York') %>% 
  arrange(weekend)

# library(ggplot2)

# 
# ny$weekend <- as.POSIXct(cal$weekend)
# 
# colnames(ny)[c(4,5)] <- c( "activity_level_ny","activity_level_label_ny")
# 
# cal$weekend <- as.POSIXct(cal$weekend)
# 
# date <- data.frame(
#   date= seq(from = as.POSIXct('2014-01-01', "%Y-%m-%d", tz="UTC"), 
#             to = as.POSIXct('2018-04-25', "%Y-%m-%d", tz="UTC"), 
#             by = 'day'))


# san_daily_summary$date <- as.POSIXct(san_daily_summary$date)

# v2$calendar <- as.POSIXct(v2$calendar)


# patient_volume <- readxl::read_xlsx('/Users/lync/UCSD/UCSD/Capstone Project/Patient_Volume.xlsx')

# patient_volume$ADT_ARRIVAL_DATE <- as.POSIXct(patient_volume$ADT_ARRIVAL_DATE)
# 
# whole <- left_join(date,cal %>% select(activity_level,activity_level_label,weekend),by=c("date"="weekend")) %>% 
#   left_join(san_daily_summary,by="date") %>% 
#   left_join(v2,by=c('date'='calendar')) %>% 
#   fill(activity_level,activity_level_label,.direction = "up") %>% 
#   left_join(patient_volume,by=c("date"="ADT_ARRIVAL_DATE")) %>% 
#   left_join(ny %>% select(activity_level_ny,activity_level_label_ny,weekend),by=c("date"="weekend")) %>% 
#   fill(activity_level_ny,activity_level_label_ny,.direction = "up") %>% 
#   mutate(weekday=wday(date),
#          month=month(date))
# 
# whole$weekday <- as.factor(whole$weekday)
# whole$month <- as.factor(whole$month)

#------------------------------
# Accesses Weather Data from the Iowa Environment Mesonet
# Allows to get weather data from Automated Surface Observing System (ASOS) stations (airports) in the whole world thanks to the Iowa Environment Mesonet website.
# • station: three or four character site identifier
#• valid: timestamp of the observation (UTC)
#• tmpf: Air Temperature in Fahrenheit, typically @ 2 meters
#• dwpf: Dew Point Temperature in Fahrenheit, typically @ 2 meters
#• relh: Relative Humidity in
#• drct: Wind Direction in degrees from north
#• sknt: Wind Speed in knots
#• p01i: One hour precipitation for the period from the observation time to the time of the previous
#hourly precipitation reset. This varies slightly by site. Values are in inches. This value may or may not contain frozen precipitation melted by some device on the sensor or estimated
# by some other means. Unfortunately, we do not know of an authoritative database denoting
#which station has which sensor.
#• alti: Pressure altimeter in inches
#• mslp: Sea Level Pressure in millibar
#• vsby: Visibility in miles
#• gust: Wind Gust in knots

#install.packages('riem')
library(riem)
riem_networks()
riem_stations("CA_ASOS")
## SAN or SDM or MYF
san <- riem_measures("SAN", date_start = "2014-01-01")
# saveRDS(san, "app/model/san.rds")
# san <- readRDS("san.rds")
san_daily_summary <- san %>% 
  dplyr::mutate(date=as.Date(valid)) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(max_temp=max(tmpf),
                   min_temp=min(tmpf),
                   avg_temp=mean(tmpf),
                   max_relh=max(relh),
                   min_relh=min(relh),
                   avg_relh=mean(relh),
                   max_sknt=max(sknt),
                   min_sknt=min(sknt),
                   avg_sknt=mean(sknt),
                   max_vsby=max(vsby),
                   min_vsby=min(vsby),
                   avg_vsby=mean(vsby))
