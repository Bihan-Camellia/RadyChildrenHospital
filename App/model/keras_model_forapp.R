# chongxinpao: hourly daily patient with total speciman and positive
library(keras)
library(kerasR)

create_dataset <- function(dataset,
                           look_back)
{l <- length(dataset)
dataX <- array(dim = c(l - look_back, look_back))
for (i in 1:ncol(dataX)){
  dataX[, i] <- dataset[i:(l - look_back + i - 1)]
}
dataY <- array(data = dataset[(look_back + 1):l],
               dim = c(l - look_back, 1))
return(
  list(
    dataX = dataX,
    dataY = dataY)
)
}

# model_daily_patient_volume_keras: daily patient
# load("app/model/lmodel/daily_patient_volume_keras_model.rda")
# 
# # model_total_specimen: weekly specimen
# load("app/model/lmodel/cdc_keras_model_total_specimen.rda")
# 
# # cdc_model: ARIMA positive cdc
# load("app/model/lmodel/cdc_weekly_percent_positive.rda")

###################################################################
# take all historical patient_volume data, daily
# predict the following 120 days.
# patient_volume <- readxl::read_excel("app/model/Patient_Volume.xlsx") #1567
# pv2 <- patient_volume[1:1300,]

daily_model <- function(volume, look_back){
  max_value <- max(volume$number)
  min_value <- min(volume$number)
  spread <- max_value - min_value
  dataset <- (volume$number - min_value) / spread #1567
  # look_back <- 120
  length_train <- length(volume$ADT_ARRIVAL_DATE)-look_back
  train <- dataset[1:length_train]
  trainXY <- create_dataset(train, look_back)
  dim_train <- dim(trainXY$dataX)
  # reshape input to be [samples, time steps, features]
  dim(trainXY$dataX) <- c(dim_train[1], 1, dim_train[2])
  model_daily_patient_volume_keras <- keras_model_sequential()
  model_daily_patient_volume_keras %>%
    layer_lstm(
      units = 4,
      input_shape = c(1, look_back)) %>%
    layer_dense(
      units = 1) %>%
    compile(
      loss = 'mean_squared_error',
      optimizer = 'adam') %>%
    fit(trainXY$dataX,
        trainXY$dataY,
        epochs = 40,
        batch_size = 1,
        verbose = 2)
}

pred_keras_d <- function(volume, look_back){
  volume <- pv2
  look_back <- 120
  max_value <- max(volume$number)
  min_value <- min(volume$number)
  spread <- max_value - min_value
  dataset <- (volume$number - min_value) / spread #1567
  length_train <- length(volume$ADT_ARRIVAL_DATE)-look_back
  test<- c(dataset[(length_train+1):length(volume$ADT_ARRIVAL_DATE)],rep(0,look_back))
  testXY <-  create_dataset(test, look_back)
  dim_test <- dim(testXY$dataX)
  dim(testXY$dataX) <- c(dim_test[1], 1, dim_test[2])
  testPredict <- model_daily_patient_volume_keras %>%
    predict(
      testXY$dataX,
      verbose = 2)
  testPredict <- testPredict * spread + min_value
  future_date <- as.Date(volume$ADT_ARRIVAL_DATE[(length_train+1):length(volume$ADT_ARRIVAL_DATE)]) + look_back
  predi <- data.frame(future_date,testPredict)
  predi
}

################################### test daily model functions

# model_daily_patient_volume_keras <- daily_model(pv2, 120)
# predict_keras_daily <- pred_keras_d(pv2, 120)
# 
# date1 <- as.Date('2017-11-20')
# pred_daily(date1,patient_volume)
# 
# patient_volume$number[which(patient_volume$ADT_ARRIVAL_DATE == date1)]
# pred_hourly(date1, predict_keras_daily$testPredict[which(predict_keras_daily$future_date == date1)])

####################################################### total specimans
# Predict total specimens with keras
#
# ili_2016 <-who_nrevss(region = c("state"), years = 2016)$clinical_labs %>%
#   filter(region=="California") %>%
#   dplyr::select(year,week,total_specimens,percent_positive,wk_date)
# 
# # write_csv(weekly_ili, "app/model/weekly_ili.csv")
# weekly_ili <- read.csv("app/model/weekly_ili.csv") %>%
#   filter(year >= 2014)
# weekly_ili$total_specimens <- as.numeric(weekly_ili$total_specimens)
# weekly_ili$percent_positive <- as.numeric(weekly_ili$percent_positive)
# 
# 
# max_value <- max(weekly_ili$total_specimens)
# min_value <- min(weekly_ili$total_specimens)
# spread <- max_value - min_value
# 
# dataset <- (weekly_ili$total_specimens - min_value) / spread
# 
# train <- dataset[1:208]
# test<- c(dataset[209:length(dataset)], rep(0,18))
# 
# look_back <- 18
# trainXY <- create_dataset(train, look_back)
# testXY <-  create_dataset(test, look_back)
# 
# dim_train <- dim(trainXY$dataX)
# dim_test <- dim(testXY$dataX)
# 
# # reshape input to be [samples, time steps, features]
# dim(trainXY$dataX) <- c(dim_train[1], 1, dim_train[2])
# dim(testXY$dataX) <- c(dim_test[1], 1, dim_test[2])
# 
# model_total_specimen <- keras_model_sequential()
# 
# model_total_specimen %>%
#   layer_lstm(
#     units = 4,
#     input_shape = c(1, look_back)) %>%
#   layer_dense(
#     units = 1) %>%
#   compile(
#     loss = 'mean_squared_error',
#     optimizer = 'adam') %>%
#   fit(trainXY$dataX,
#       trainXY$dataY,
#       epochs = 10,
#       batch_size = 1,
#       verbose = 2)
# 
# 
# testPredict <- model_total_specimen %>%
#   predict(
#     testXY$dataX,
#     verbose = 2)
# testPredict_total_specimen <- testPredict * spread + min_value
# weekly_ili$wk_date[209:235]
# testPredict <- specimen %>%
#   predict(
#     testXY$dataX,
#     verbose = 2)

####################################################### positive


