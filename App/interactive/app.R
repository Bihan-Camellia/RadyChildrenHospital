library(shiny)
library(pool)
library(glue)
library(DBI)
library(RPostgreSQL)
library(RSQLite)
library(DT)
library(dplyr)
library(plotly)
source('functions.R')
source('database.R')


###########

# build local SQLite DB and store all pre-defined dataframes
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


ui <- fluidPage(
  title = 'Staffing Schedule',
  sidebarLayout(
    sidebarPanel(
      'Select season and weekday that you want to schedule:',
      br(),
      selectInput('season', 'Select month:', choices = c('High', 'Medium', 'Low')),
      selectInput('weekday', 'Select weekday:', choices = c('Sun/Mon', 'Tue-Sat')),
      uiOutput("buttons"),
      hr(),
      'Press Analyze to show plot',
      br(),
      br(),
      actionButton('run', 'Analyze'),
      width = 3
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Acuities 1-2-3', 
                           numericInput('Pts123', label='Number of high acuity patients per hour: ', value = 2.15, step = 0.05, min = 0),
                           sliderInput('serviceLevelH', label='What percentile do you want:', value = 0.75, max = 1, min = 0),
                           dataTableOutput('highAcuity'), 
                           plotOutput('highplot')),
                  tabPanel('Acuities 4-5', 
                           numericInput('Pts45', label='Number of low acuity patients per hour: ', value = 3.00, step = 0.05, min = 0),
                           sliderInput('serviceLevelL', label='What percentile do you want:', value = 0.75, max = 1, min = 0),
                           dataTableOutput('lowAcuity'), 
                           plotOutput('lowplot')),
                  tabPanel('Total', plotOutput('totalplot')))
    )
  )
)

server <- function(input, output){
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
    j = info$col = info$col + 1  # column index offset by 1
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
  
  observeEvent(input$save, {
    updateDB(editedValue = rvs$editedInfo, pool = pool, tbl = get_high_slot(input$season, input$weekday))
    updateDB(editedValue = rvs$editedInfo, pool = pool, tbl = get_low_slot(input$season, input$weekday))
    # updateDB(editedValue2 = rvs$editedInfo2, pool = pool, tbl = "hhh")
    rvs$dbhigh <- rvs$high
    rvs$dblow <- rvs$low
    rvs$data1Same <- TRUE
    rvs$data2Same <- TRUE
  })
  
  observeEvent(input$cancel, {
    rvs$high <- rvs$dbhigh
    rvs$low <- rvs$dblow
    rvs$data1Same <- TRUE
    rvs$data2Same <- TRUE
  })
  
  output$buttons <- renderUI({
    div(
      if (! (rvs$data1Same & rvs$data2Same)) {
        span(
          hr(),
          'Press store to store your schedule',
          'Press back to return to last stored schedule',
          actionButton(inputId = "save", label = "Store",
                       class = "btn-primary"),
          actionButton(inputId = "cancel", label = "Back")
        )
      } else {
        span()
      }
    )
  })
  
  datPassH <- reactive({
    datpassh <- PtVolume %>% 
      summarise(numPercentile = quantile(numPt, input$serviceLevelH)) %>% 
      ungroup() %>% 
      filter(SEASON == input$season,
           ACUITY == 'High',
           WEEKDAY == input$weekday) %>% 
    select(ADT_HOUR, numPercentile) %>%
    mutate(group = rep(1:8, each = 3)) %>%
    group_by(group) %>%
    mutate(smooth = mean(numPercentile)) %>%
    ungroup() %>%
    select(ADT_HOUR, smooth)
    datpassh <- datpassh %>% mutate(staffingNeed = calcStaff(datpassh) / input$Pts123)
  })
  
  datPassL <- reactive({
    datpassl <- PtVolume %>% 
      summarise(numPercentile = quantile(numPt, input$serviceLevelL)) %>% 
      ungroup() %>% 
    filter(SEASON == input$season,
           ACUITY == 'Low',
           WEEKDAY == input$weekday) %>% 
    select(ADT_HOUR, numPercentile) %>% 
    mutate(group = rep(1:8, each = 3)) %>% 
    group_by(group) %>% 
    mutate(smooth = mean(numPercentile)) %>% 
    ungroup() %>% 
    select(ADT_HOUR, smooth)
    datpassl <- datpassl %>% mutate(staffingNeed = calcStaff(datpassl) / input$Pts123)
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
                            staffingPlanH = colSums(rvs$high[-1:-2], na.rm = TRUE),
                            patientEstimateL = rvs$dpassL$smooth,
                            staffingNeedL = rvs$dpassL$staffingNeed,
                            staffingPlanL = colSums(rvs$low[-1:-2], na.rm = TRUE),
                            staffingDiffH = - staffingNeedH + staffingPlanH,
                            staffingDiffL = - staffingNeedL + staffingPlanL,
                            totalPtEst = patientEstimateH + patientEstimateL,
                            totalStaffingNeed = staffingNeedH + staffingNeedL,
                            totalStaffingPlan = staffingPlanH + staffingPlanL,
                            totalStaffingDiff = - totalStaffingNeed + totalStaffingPlan)
  })
  
  observeEvent(input$run, {
    output$highplot <- renderPlot({
      make_plot(df(), 'staffingPlanH', 'staffingNeedH')
    })
  })
  
  observeEvent(input$run, {
    output$lowplot <- renderPlot({
      make_plot(df(), 'staffingPlanL','staffingNeedL')
    })
  })
  
  observeEvent(input$run, {
    output$totalplot <- renderPlot({
      make_plot(df(), 'totalStaffingPlan', 'totalStaffingNeed')
    })
  })
  
}

shinyApp(ui, server)


