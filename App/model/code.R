library(plotly)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(keras)
library(kerasR)

lwbs_rate_df <- read_csv('lwbs/data/lwbs_rate.csv') %>% select(-X1)
lwbs_time_df <- read_csv('lwbs/data/lwbs_time.csv') %>% select(-X1)

make_html <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n") %>%
    knitr::knit2html(
      text = ., fragment.only = TRUE, quiet = TRUE,
      options = "", stylesheet = ""
    ) %>%
    HTML() %>%
    gsub("&lt;!--/html_preserve--&gt;", "", .) %>%
    gsub("&lt;!--html_preserve--&gt;", "", .) %>%
    gsub("&lt;!&ndash;html_preserve&ndash;&gt;", "", .) %>%
    gsub("&lt;!&ndash;/html_preserve&ndash;&gt;", "", .) %>%
    withMathJax()
}


pp <- function(df){
  df %>%
    ggplot(aes(x = ADT_ARRIVAL_DATE, y = number)) +
    geom_line(color = '#3c8dbc', alpha = 0.75, size = 0.5) +
    xlab("Patient Arrival Date")+
    ylab("Total Number") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank())
  ggplotly() %>%
    style(text=hover, hoverinfo = "text")
}

##################################
####model
# weekday weeknumber keras*3

load("model/lmodel/geiemily_daily.rda")
load("model/lmodel/geiemily_hourly.rda")
# load("model/lmodel/chongxinpao.rda")

# user can update the excel dataset with column name: ADT_ARRIVAL_DATE, number
# patient_volume <- readxl::read_excel("model/Patient_Volume.xlsx")
# patient_volume$ADT_ARRIVAL_DATE <- as.Date(patient_volume$ADT_ARRIVAL_DATE)
# predict(model, patient_volume)
pred_daily <- function(date, volume){
  input_date <- as.Date(date)
  patient_volume <- volume
  patient_volume$ADT_ARRIVAL_DATE <- as.Date(patient_volume$ADT_ARRIVAL_DATE)
  have_date <- input_date-120
  number <- patient_volume$number[which(patient_volume$ADT_ARRIVAL_DATE ==have_date)]
  weeknumber <- as.factor(week(input_date))
  weekday <- as.factor(wday(input_date))
  month <- as.factor(month(input_date))
  daily <- data.frame(weeknumber, number, weekday, month)
  pred <- predict(result_lag_lala, daily)
  round(pred$Prediction)
}

# hourly
# hour daily_volume, weeknumber, weekday

pred_hourly <- function(date, daily, ptvolume){
  hour <- as.factor(seq(0,23,1))
  daily_volume <- rep(daily,24)
  input_date <- date
  weeknumber <- rep(as.factor(week(input_date)), 24)
  weekday <- rep(as.factor(wday(input_date)),24)
  
  hourly <- data.frame(hour, daily_volume, weeknumber, weekday)
  
  pred <- predict(lala, hourly)

  PtVolume <- ptvolume
  three <- PtVolume %>%
    group_by(ADT_ARRIVAL_DATE, ADT_HOUR) %>%
    summarize(pt = sum(numPt)) %>%
    filter(ADT_ARRIVAL_DATE == as.POSIXct(input_date, tz="UTC")-86400,
           ADT_HOUR %in% c(23,22,21,20))

  p <- rollapply(c(three$pt, pred$Prediction), 4, sum)

  df <- data.frame(pred.hour = pred$hour, pred = p)
  df
}


pp_plot <- function(df){
  df %>%
    ggplot(aes(x = pred.hour, y = pred)) +
    geom_bar(stat = "identity", fill = '#3c8dbc') +
    geom_errorbar(aes(ymin = totalStaffingPlan, ymax = totalStaffingPlan), color = 'red', size =3) +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_y_continuous(limits = c(0,20),
                       breaks = seq(0,20,2))
}

#################################################
# daily patient volume use keras model
source('model/keras_model_forapp.R')



