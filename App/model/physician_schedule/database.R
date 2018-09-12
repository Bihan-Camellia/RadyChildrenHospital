library(readr)
serviceLevel <- 0.75
# PtVolume <- read_csv('model/physician_schedule/PtVolume.csv')%>% 
#   mutate(WEEKDAY = plyr::mapvalues(WEEKDAY, from=c('Mon/Sun','Tue/Sat'), to=c('Sun/Mon', 'Tue-Sat'))) %>% 
#   group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY)
# head(PtVolume)


#### setup pre-defined schedule dataframes
timeslot <- data.frame(H0 = rep(NA, 10),
                       H1 = rep(NA, 10),
                       H2 = rep(NA, 10),
                       H3 = rep(NA, 10),
                       H4 = rep(NA, 10),
                       H5 = rep(NA, 10),
                       H6 = rep(NA, 10),
                       H7 = rep(NA, 10),
                       H8 = rep(NA, 10),
                       H9 = rep(NA, 10),
                       H10 = rep(NA, 10),
                       H11 = rep(NA, 10),
                       H12 = rep(NA, 10),
                       H13 = rep(NA, 10),
                       H14 = rep(NA, 10),
                       H15 = rep(NA, 10),
                       H16 = rep(NA, 10),
                       H17 = rep(NA, 10),
                       H18 = rep(NA, 10),
                       H19 = rep(NA, 10),
                       H20 = rep(NA, 10),
                       H21 = rep(NA, 10),
                       H22 = rep(NA, 10),
                       H23 = rep(NA, 10))

timeslot <- as.data.frame(sapply(timeslot, as.numeric))

# lhh <- data.frame(
#   Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Float'),
#   Time = c('6a-3p', '7a-4p', '2p-11p', '3p-12a', '10p-6a', '11p-7a', '9p-5p', '5p-1a', '4p-10p')
# ) %>% bind_cols(head(timeslot, nrow(.)))

lhh <- data.frame(
  Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Float'),
  Time = rep(NA, 9),
  Beds = rep(NA, 9)
) %>% bind_cols(head(timeslot, nrow(.)))

# llh <- data.frame(
#   Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Float'),
#   Time = c('6a-3p', '7a-4p', '2p-11p', '3p-12a', '10p-6a', '11p-7a', '12p-6p', '6p-1a (Wed-Sat)', '4p-10p')
# ) %>% bind_cols(head(timeslot, nrow(.)))

llh <- data.frame(
  Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Float'),
  Time = rep(NA, 9),
  Beds = rep(NA, 9)
) %>% bind_cols(head(timeslot, nrow(.)))

# lhl <- data.frame(
#   Zone = c('FT am', 'FT pm', 'FT ON', 'P', 'P', 'EDS1 ', 'EDS2'),
#   Time = c('9a-4p', '4p-10p', '9:30p-4a', '10:30a-6:30p', '6p-2a', '5:30p-1:30a (weekdays)', 'SS (Sun-Tues)')
# ) %>% bind_cols(head(timeslot, nrow(.)))
# 
# lll <- data.frame(
#   Zone = c('FT am', 'FT pm', 'FT ON', 'P', 'P', 'EDS1 ', 'EDS2'),
#   Time = c('9a-4p', '4p-10p', '9:30p-4a', '10:30a-6:30p', '6p-2a', '5:30p-1:30a (weekdays)', '')
# ) %>% bind_cols(head(timeslot, nrow(.)))

lhl <- data.frame(
  Zone = c('FT am', 'FT pm', 'FT ON', 'P', 'P', 'EDS1 ', 'EDS2'),
  Time = rep(NA, 7),
  Beds = rep(NA, 7)
  # Time = c('9a-4p', '4p-10p', '9:30p-4a', '10:30a-6:30p', '6p-2a', '5:30p-1:30a (weekdays)', 'SS (Sun-Tues)')
) %>% bind_cols(head(timeslot, nrow(.)))

lll <- data.frame(
  Zone = c('FT am', 'FT pm', 'FT ON', 'P', 'P', 'EDS1 ', 'EDS2'),
  Time = rep(NA, 7),
  Beds = rep(NA, 7)
  # Time = c('9a-4p', '4p-10p', '9:30p-4a', '10:30a-6:30p', '6p-2a', '5:30p-1:30a (weekdays)', '')
) %>% bind_cols(head(timeslot, nrow(.)))

######

mhh <- data.frame(
  Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Orange', 'Float'),
  Time = rep(NA, 10),
  Beds = rep(NA, 10)
  # Time = c('6a-3p', '7a-4p', '2p-11p', '3p-12a', '10p-6a', '11p-7a', '6a-12p', '12p-7:30p', '7p-2a', '9a-3p')
) %>% bind_cols(head(timeslot, nrow(.)))

mlh <- data.frame(
  Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Orange', 'Float'),
  Time = rep(NA, 10),
  Beds = rep(NA, 10)
  # Time = c('6a-3p', '7a-4p', '2p-11p', '3p-12a', '10p-6a', '11p-7a', '6a-12p', '12p-7:30p', '7p-2a', '9a-3p')
) %>% bind_cols(head(timeslot, nrow(.)))

mhl <- data.frame(
  Zone = c('FT am', 'FT pm', 'FT ON', 'Float', 'P', 'P', 'EDS1 ', 'EDS2'),
  Time = rep(NA, 8),
  Beds = rep(NA, 8)
  # Time = c('9a-4p', '4p-10p', '9:30p-4a', '3p-11p', '10:30a-6:30p', '6p-2a', '5:30p-1:30a (weekdays)', '5:30p-11p')
) %>% bind_cols(head(timeslot, nrow(.)))

mll <- data.frame(
  Zone = c('FT am', 'FT pm', 'FT ON', 'Float', 'P', 'P', 'EDS1 ', 'EDS2'),
  Time = rep(NA, 8),
  Beds = rep(NA, 8)
  # Time = c('9a-4p', '4p-10p', '9:30p-4a', '3p-11p', '10:30a-6:30p', '6p-2a', '5:30p-1:30a (weekdays)', '5:30p-11p')
) %>% bind_cols(head(timeslot, nrow(.)))

######

hhh <- data.frame(
  Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Orange', 'Float'),
  Time = rep(NA, 10),
  Beds = rep(NA, 10)
  # Time = c('6a-3p', '7a-4p', '2p-11p', '3p-12a', '10p-6a', '11p-7a', '6a-12p', '12p-7p', '7p-2a', '9a-3p')
) %>% bind_cols(head(timeslot, nrow(.)))

hlh <- data.frame(
  Zone = c('Y1', 'Y2', 'Y1', 'Y2', 'Y1', 'Y2', 'Orange', 'Orange', 'Orange', 'Float'),
  Time = rep(NA, 10),
  Beds = rep(NA, 10)
  # Time = c('6a-3p', '7a-4p', '2p-11p', '3p-12a', '10p-6a', '11p-7a', '6a-12p', '12p-7p', '7p-2a', '9a-3p')
) %>% bind_cols(head(timeslot, nrow(.)))

hhl <- data.frame(
  Zone = c('FT am', 'FT pm', 'FT ON', 'P', 'P', 'EDS1 ', 'EDS2'),
  Time = rep(NA, 7),
  Beds = rep(NA, 7)
  # Time = c('9a-4p', '4p-10p', '9:30p-4a', '10a-6p', '6p-2a', '5:30p-1:30a (weekdays)', '5p-12p')
) %>% bind_cols(head(timeslot, nrow(.)))

hll <- data.frame(
  Zone = c('FT am', 'FT pm', 'FT ON', 'P', 'P', 'EDS1 ', 'EDS2'),
  Time = rep(NA, 7),
  Beds = rep(NA, 7)
  # Time = c('9a-4p', '4p-10p', '9:30p-4a', '10a-6p', '6p-2a', '5:30p-1:30a (weekdays)', '5p-12p')
) %>% bind_cols(head(timeslot, nrow(.)))
