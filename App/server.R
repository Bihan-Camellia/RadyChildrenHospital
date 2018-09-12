library(tidyverse)
library(shiny)
library(shinydashboard)
library(pool)
library(glue)
library(DBI)
library(RPostgreSQL)
library(RSQLite)
library(DT)
library(dplyr)
library(plotly)
library(keras)
library(kerasR)
library(zoo)

#### remember to CHECK PATH when running in CITRIX
source("model/Vacation_data.R")
source('model/physician_schedule/functions.R')
source('model/physician_schedule/database.R')
source('model/code.R')

pool <- pool::dbPool(drv = DBI::dbDriver("SQLite"),
                     dbname="Schedule",
                     host="localhost",
                     user= 'RCHdemo',
                     password="goodluck")

conn <- poolCheckout(pool)

for(df in ls(.GlobalEnv)){
  if(class(get(df))[1] == 'data.frame'){
    print(df)
    dbWriteTable(conn, name=df,value=get(df), overwrite=TRUE)
  }
}

dbListTables(conn)
poolReturn(conn)

######

onStop(function(){
  poolClose(pool)
})

#######

server <- function(input, output){

  set.seed(18)

  ######### staffing schedule
  rvs <- reactiveValues(
    high = NA,
    dbhigh = NA,
    data1Same = TRUE,
    low = NA,
    dblow = NA,
    data2Same = TRUE,
    editedInfo = NA,
    dpassH = NA,
    dpassL = NA
  )
  
  get_high <- reactive({
    req(input$season, input$weekday)
    pool %>% tbl(get_high_slot(input$season, input$weekday)) %>% collect()
    
  })
  get_low <- reactive({
    req(input$season, input$weekday)
    pool %>% tbl(get_low_slot(input$season, input$weekday)) %>% collect()
  })

  observeEvent(get_high(),{
    high <-get_high()
    rvs$high <- high
    rvs$dbhigh <- high
  })
  
  observeEvent(get_low(),{
    low <- get_low()
    rvs$low <- low
    rvs$dblow <- low
  })
  
  output$highAcuity <- DT::renderDataTable(
    rvs$high, editable = TRUE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      lengthChange = FALSE,
      searching = FALSE,
      bInfo = FALSE,
      paging = FALSE
    )
  )
  
  output$lowAcuity <- DT::renderDataTable(
    rvs$low, editable = TRUE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      lengthChange = FALSE,
      searching = FALSE,
      bInfo = FALSE,
      paging = FALSE
    )
  )
  
  proxyhigh = dataTableProxy('highAcuity')
  
  observeEvent(input$highAcuity_cell_edit, {
    
    info = input$highAcuity_cell_edit
    
    i = info$row
    j = info$col = info$col + 1  # column index offset by 1
    v = info$value
    
    info$value <- as.numeric(info$value)
    
    rvs$high[i, j] <<- DT::coerceValue(v, purrr::flatten_dbl(rvs$high[i, j]))
    replaceData(proxyhigh, rvs$high, resetPaging = FALSE, rownames = FALSE)
    
    rvs$data1Same <- identical(rvs$high, rvs$dbhigh)
    
    if (all(is.na(rvs$editedInfo))) {
      rvs$editedInfo <- data.frame(info)
    } else {
      rvs$editedInfo <- dplyr::bind_rows(rvs$editedInfo, data.frame(info))
    }
  })
  
  proxylow = dataTableProxy('lowAcuity')
  
  observeEvent(input$lowAcuity_cell_edit, {
    
    info = input$lowAcuity_cell_edit
    
    i = info$row
    j = info$col = info$col   # column index offset by 1
    v = info$value
    
    info$value <- as.numeric(info$value)
    
    rvs$low[i, j] <<- DT::coerceValue(v, purrr::flatten_dbl(rvs$low[i, j]))
    replaceData(proxylow, rvs$low, resetPaging = FALSE, rownames = FALSE)
    
    rvs$data1Same <- identical(rvs$low, rvs$dblow)
    
    if (all(is.na(rvs$editedInfo))) {
      rvs$editedInfo <- data.frame(info)
    } else {
      rvs$editedInfo <- dplyr::bind_rows(rvs$editedInfo, data.frame(info))
    }
  })
  
  ############################################model
  # rvs <- reactiveValues(
  #   high_lm = NA,
  #   dbhigh_lm = NA,
  #   data1Same_lm = TRUE,
  #   low_lm = NA,
  #   dblow_lm = NA,
  #   data2Same_lm = TRUE,
  #   editedInfo_lm = NA,
  #   dpassH_lm = NA,
  #   dpassL_lm = NA
  # )
  # observeEvent(get_high(),{
  #   high <-get_high()
  #   rvs$high_lm <- high
  #   rvs$dbhigh_lm <- high
  # })
  # 
  # observeEvent(get_low(),{
  #   low <- get_low()
  #   rvs$low_lm <- low
  #   rvs$dblow_lm <- low
  # })
  output$hAcuity <- DT::renderDataTable(
    rvs$high, editable = TRUE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      lengthChange = FALSE,
      searching = FALSE,
      bInfo = FALSE,
      paging = FALSE
    )
  )
  
  proxyhigh = dataTableProxy('hAcuity')
  
  observeEvent(input$hAcuity_cell_edit, {
    
    info = input$hAcuity_cell_edit
    
    i = info$row
    j = info$col = info$col + 1  # column index offset by 1
    v = info$value
    
    info$value <- as.numeric(info$value)
    
    rvs$high[i, j] <<- DT::coerceValue(v, purrr::flatten_dbl(rvs$high[i, j]))
    replaceData(proxyhigh, rvs$high, resetPaging = FALSE, rownames = FALSE)
    
    rvs$data1Same <- identical(rvs$high, rvs$dbhigh)
    
    if (all(is.na(rvs$editedInfo))) {
      rvs$editedInfo <- data.frame(info)
    } else {
      rvs$editedInfo <- dplyr::bind_rows(rvs$editedInfo, data.frame(info))
    }
  })
  output$lAcuity <- DT::renderDataTable(
    rvs$low, editable = TRUE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      lengthChange = FALSE,
      searching = FALSE,
      bInfo = FALSE,
      paging = FALSE
    )
  )
  proxylow = dataTableProxy('lAcuity')
  
  observeEvent(input$lAcuity_cell_edit, {
    
    info = input$lAcuity_cell_edit
    
    i = info$row
    j = info$col = info$col   # column index offset by 1
    v = info$value
    
    info$value <- as.numeric(info$value)
    
    rvs$low[i, j] <<- DT::coerceValue(v, purrr::flatten_dbl(rvs$low[i, j]))
    replaceData(proxylow, rvs$low, resetPaging = FALSE, rownames = FALSE)
    
    rvs$data1Same <- identical(rvs$low, rvs$dblow)
    
    if (all(is.na(rvs$editedInfo))) {
      rvs$editedInfo <- data.frame(info)
    } else {
      rvs$editedInfo <- dplyr::bind_rows(rvs$editedInfo, data.frame(info))
    }
  })
  #####################################################################################

  
  datPassH <- reactive({
    req(input$patient_file)
    PtVolume <- read_csv(input$patient_file$datapath)%>% 
      mutate(WEEKDAY = plyr::mapvalues(WEEKDAY, from=c('Mon/Sun','Tue/Sat'), to=c('Sun/Mon', 'Tue-Sat'))) %>% 
      group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY)
    datpassh <- PtVolume %>% 
      dplyr::summarise(numPercentile = quantile(numPt, input$serviceLevelH)) %>% 
      ungroup() %>% 
      dplyr::filter(SEASON == input$season,
             ACUITY == 'High',
             WEEKDAY == input$weekday) %>% 
      dplyr::select(ADT_HOUR, numPercentile) %>%
      mutate(group = rep(1:8, each = 3)) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(smooth = mean(numPercentile)) %>%
      ungroup() %>%
      dplyr::select(ADT_HOUR, smooth)
    datpassh <- datpassh %>% dplyr::mutate(staffingNeed = calcStaff(datpassh) / input$Pts123)
  })
  
  datPassL <- reactive({
    req(input$patient_file)
    PtVolume <- read_csv(input$patient_file$datapath)%>% 
      mutate(WEEKDAY = plyr::mapvalues(WEEKDAY, from=c('Mon/Sun','Tue/Sat'), to=c('Sun/Mon', 'Tue-Sat'))) %>% 
      group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY)
    datpassl <- PtVolume %>% 
      dplyr::summarise(numPercentile = quantile(numPt, input$serviceLevelL)) %>% 
      ungroup() %>% 
      dplyr::filter(SEASON == input$season,
             ACUITY == 'Low',
             WEEKDAY == input$weekday) %>% 
      dplyr::select(ADT_HOUR, numPercentile) %>% 
      dplyr::mutate(group = rep(1:8, each = 3)) %>% 
      dplyr::group_by(group) %>% 
      dplyr::mutate(smooth = mean(numPercentile)) %>% 
      ungroup() %>% 
      dplyr::select(ADT_HOUR, smooth)
    datpassl <- datpassl %>% dplyr::mutate(staffingNeed = calcStaff(datpassl) / input$Pts123)
  })
  
  observeEvent(datPassH(),{
    dpassH <- datPassH()
    rvs$dpassH <- dpassH
  })
  
  observeEvent(datPassL(),{
    dpassL <- datPassL()
    rvs$dpassL <- dpassL
  })
  
  
  df <- reactive({
    dataframe <- data_frame(hour = factor(seq(0,23,1)),
                            patientEstimateH = rvs$dpassH$smooth,
                            staffingNeedH = rvs$dpassH$staffingNeed,
                            staffingPlanH = colSums(rvs$high[-1:-3], na.rm = TRUE),
                            patientEstimateL = rvs$dpassL$smooth,
                            staffingNeedL = rvs$dpassL$staffingNeed,
                            staffingPlanL = colSums(rvs$low[-1:-3], na.rm = TRUE),
                            staffingDiffH = - staffingNeedH + staffingPlanH,
                            staffingDiffL = - staffingNeedL + staffingPlanL,
                            totalPtEst = patientEstimateH + patientEstimateL,
                            totalStaffingNeed = staffingNeedH + staffingNeedL,
                            totalStaffingPlan = staffingPlanH + staffingPlanL,
                            totalStaffingDiff = - totalStaffingNeed + totalStaffingPlan)
  })
  
  patient_volume <- reactive({
    p <- readxl::read_excel(input$file$datapath)
    max(as.Date(p$ADT_ARRIVAL_DATE))
  })
  
  daily_volmue <- reactive({
    req(input$date_want)
    patient_volume <- readxl::read_excel(input$file$datapath)
    daily <- pred_daily(input$date_want, patient_volume)
    daily
  })
  
  lm_df <- reactive({
    req(input$date_want)
    req(input$patient_file)
    PtVolume <- read_csv(input$patient_file$datapath)%>% 
      mutate(WEEKDAY = plyr::mapvalues(WEEKDAY, from=c('Mon/Sun','Tue/Sat'), to=c('Sun/Mon', 'Tue-Sat'))) %>% 
      group_by(ADT_HOUR, ACUITY, SEASON, WEEKDAY)
    
    patient_volume <- readxl::read_excel(input$file$datapath)
    daily <- pred_daily(input$date_want, patient_volume)
    temp <- pred_hourly(input$date_want, daily, PtVolume)
    temp$staffingPlanH <- colSums(rvs$high[-1:-3], na.rm = TRUE)
    temp$staffingPlanL <- colSums(rvs$low[-1:-3], na.rm = TRUE)
    temp <- temp %>% mutate(totalStaffingPlan = staffingPlanH + staffingPlanL)
  })
  
 
    output$highplot <- renderPlot({
      validate(
        need(input$patient_file, "Please upload the CSV file of Historical Patient Volume to see the hourly patient volume distribution.")    
      )
      make_plot(df(), 'staffingPlanH', 'staffingNeedH')
    })

    output$lowplot <- renderPlot({
      validate(
        need(input$patient_file, "Please upload the CSV file of Historical Patient Volume to see the hourly patient volume distribution.")    
      )
      make_plot(df(), 'staffingPlanL','staffingNeedL')
    })

  
    output$totalplot <- renderPlot({
      validate(
        need(input$patient_file, "Please upload the CSV file of Historical Patient Volume to see the hourly patient volume distribution.")    
      )
      make_plot(df(), 'totalStaffingPlan', 'totalStaffingNeed')
    })

 ##################################################################### 
    ### LWBS
    
    output$generate <- renderUI({
      validate(
        need(input$lwbs_rate_date[1], 'Please enter the start date'),
        need(input$lwbs_rate_date[2], 'Please enter the end date'),
        need(input$lwbs_rate_date[1] < input$lwbs_rate_date[2], 'Make sure the start date is no later than the end date')
      )
      actionButton('lwbs_rate_show', 'Generate plot')
    })
    
    output$analyze <- renderUI({
      validate(
        need(input$threshold, 'Please enter the threshold'),
        need(input$lwbs_rate_date[1], 'Please enter the start date'),
        need(input$lwbs_rate_date[2], 'Please enter the end date'),
        need(input$lwbs_rate_date[1] < input$lwbs_rate_date[2], 'Make sure the start date is no later than the end date')
      )
      actionButton('threshold_analyze', 'Set')
    })
    
    observeEvent(input$lwbs_rate_show, {
      output$lwbs_rate_plot <- renderPlot({
        req(input$lwbs_rate_date[1], input$lwbs_rate_date[2])
        lwbs_rate_df %>%
          filter(ADT_ARRIVAL_DATE >= as.Date(input$lwbs_rate_date[1]),
                 ADT_ARRIVAL_DATE <= as.Date(input$lwbs_rate_date[2])) %>% 
          ggplot(aes(x= ADT_ARRIVAL_DATE, y = LWBS_rate, color = season)) +
          geom_point(size = 0.1) +
          geom_line(aes(group = 1)) +
          theme_bw() +
          labs(x=NULL, y = 'LWBS rate')
      })
    })
    
    
    threshold_reduce <- reactive({
      req(input$lwbs_rate_date[1], input$lwbs_rate_date[2])
      lwbs_time_df %>% 
        filter(date >= as.Date(input$lwbs_rate_date[1]),
               date <= as.Date(input$lwbs_rate_date[2])) %>% 
        filter(bedToLeft > 0) %>% 
        mutate(lessthan = bedToLeft <= input$threshold) %>% 
        summarise(count_before = n(),
                  count_after = sum(lessthan)) %>% 
        ungroup() 
    })
    
    observeEvent(input$threshold_analyze,{
      output$lwbs_reduce <- renderPlot({
        threshold_reduce() %>% 
          gather(key = type, value = count)%>% 
          ggplot(aes(x = type, y = count)) +
          geom_col() +
          theme_bw() +
          scale_x_discrete(labels=c('After','Original')) +
          labs(x=NULL,y='LWBS Count')
      })
    })
    
    observeEvent(input$threshold_analyze,{
      output$lwbs_reduce_text <- renderText({
        perc <- threshold_reduce() %>% 
          summarise(perc_dec = sum(count_before - count_after) / sum(count_before))
        paste('After putting a ', input$threshold, '-minute thresholds, the LWBS rate will reduce by ', 
              round(perc$perc_dec * 100, 2), '% for the given time frame (from ', input$lwbs_rate_date[1], ' to ', input$lwbs_rate_date[2],'). ')
      })
    })
######################################################################################
  
  getData <- reactive({
    if(is.null(input$file)) return(NULL)
    else return(input$file)
  })

  output$select_ui <- renderUI({
    validate(
      need(input$file, "Please upload the Excel file of Historical Patient Volume.")    )
    dateInput("date_want", "Select predict date:",
              max = patient_volume()+120,
              min = as.Date("2018-01-01")
              # value = as.Date("2018-07-12")
              )
    })

  output$rch_about <- renderUI({
    div(
      make_html("model/about.Rmd"),
      style = "width: 800px; margin-left: auto; margin-right: auto;"
    )
  })
  
  
  output$pred_dis <- renderPlot({
    validate(
      need(input$file, "Please upload the Excel file of 
           Historical Patient Volume to see the hourly patient volume distribution.")    )
    pp_plot(lm_df())
  })
  
  observeEvent(input$file, {
    output$daily_ui <- renderText({
      paste("Patient Volume on",input$date_want, "will be", daily_volmue(), ".")
    })
  })
  
}
