library(lubridate)
library(dplyr)
library(tidyverse)
library(timeDate)
library(data.table)

# transfer character to POSIXct format
# a <- as.POSIXct(encounters$UPDATE_DATE, origin = "1970-01-01", format = "%m/%d/%Y %H:%M")

# create time dummy variable with vacation time 
calendar <- seq.Date(from=as.Date("2014-01-01"), to=as.Date("2018-12-31"), by="day")

v <- data.frame(calendar, free = rep(0, length(calendar)))

# acadmic vacation: winter summer thanksgiving spring_break + final
t1 <- c(seq.Date(from = as.Date("2014-06-16"), to = as.Date("2014-08-29"), by = "day"),
        seq.Date(from = as.Date("2015-06-15"), to = as.Date("2015-08-28"), by = "day"),
        seq.Date(from = as.Date("2016-06-13"), to = as.Date("2016-08-26"), by = "day"),
        seq.Date(from = as.Date("2017-06-19"), to = as.Date("2017-08-25"), by = "day"),
        seq.Date(from = as.Date("2018-06-18"), to = as.Date("2018-08-31"), by = "day"),
        seq.Date(from = as.Date("2014-04-7"), to = as.Date("2014-04-11"), by = "day"),
        seq.Date(from = as.Date("2015-04-6"), to = as.Date("2015-04-10"), by = "day"),
        seq.Date(from = as.Date("2016-04-11"), to = as.Date("2016-04-15"), by = "day"),
        seq.Date(from = as.Date("2017-04-10"), to = as.Date("2017-04-14"), by = "day"),
        seq.Date(from = as.Date("2018-04-09"), to = as.Date("2018-04-13"), by = "day"),
        seq.Date(from = as.Date("2014-12-25"), to = as.Date("2015-01-9"), by = "day"),
        seq.Date(from = as.Date("2015-12-25"), to = as.Date("2016-01-8"), by = "day"),
        seq.Date(from = as.Date("2016-12-25"), to = as.Date("2017-01-6"), by = "day"),
        seq.Date(from = as.Date("2017-12-25"), to = as.Date("2018-01-5"), by = "day"),
        seq.Date(from = as.Date("2014-11-27"), to = as.Date("2014-11-28"), by = "day"),
        seq.Date(from = as.Date("2015-11-26"), to = as.Date("2015-11-27"), by = "day"),
        seq.Date(from = as.Date("2016-11-24"), to = as.Date("2016-11-25"), by = "day"),
        seq.Date(from = as.Date("2017-11-23"), to = as.Date("2017-11-24"), by = "day"),
        seq.Date(from = as.Date("2018-11-22"), to = as.Date("2018-11-23"), by = "day")
)

# legal holiday
t_2 <- fread('https://gist.githubusercontent.com/shivaas/4758439/raw/b0d3ddec380af69930d0d67a9e0519c047047ff8/US%2520Bank%2520holidays')[21:70,2]
t2 <- as.data.frame(t_2)
t2v <-as.Date(c(t2$V2))

# data: students are free
t <- unique(c(t1, t2v))

# v: include holiday, academic vacation, weekend
v1 <- v
v1$free <- ifelse(weekdays(v$calendar) == c("Saturday")
                 | weekdays(v$calendar) == c("Sunday")
                 | v$calendar %in% t, 1, 0)
#write.csv(file = "data/vacation.csv", v)


# v2: the day after holiday, final.
v2 <- data.frame(calendar, free = rep(0, length(calendar)))

final <- c(seq.Date(from = as.Date("2014-06-9"), to = as.Date("2014-06-13"), by = "day"),
        seq.Date(from = as.Date("2015-06-8"), to = as.Date("2015-06-12"), by = "day"),
        seq.Date(from = as.Date("2016-06-6"), to = as.Date("2016-06-10"), by = "day"),
        seq.Date(from = as.Date("2017-06-5"), to = as.Date("2017-06-9"), by = "day"),
        seq.Date(from = as.Date("2018-06-11"), to = as.Date("2018-06-15"), by = "day"),
        seq.Date(from = as.Date("2014-12-15"), to = as.Date("2014-12-19"), by = "day"),
        seq.Date(from = as.Date("2015-12-14"), to = as.Date("2015-12-18"), by = "day"),
        seq.Date(from = as.Date("2016-12-19"), to = as.Date("2016-12-23"), by = "day"),
        seq.Date(from = as.Date("2017-12-18"), to = as.Date("2017-12-22"), by = "day"),
        seq.Date(from = as.Date("2018-12-17"), to = as.Date("2018-12-21"), by = "day")
)

t_2 <- fread('https://gist.githubusercontent.com/shivaas/4758439/raw/b0d3ddec380af69930d0d67a9e0519c047047ff8/US%2520Bank%2520holidays')[21:70,2]
t2 <- as.data.frame(t_2)
t2v <-as.Date(c(t2$V2))

day_ah<- t2v + 1

panic <- unique(c(day_ah, final))

v2$free <- ifelse(v2$calendar %in% panic, 1, 0)
#write.csv(file = "data/panic.csv", v2)
