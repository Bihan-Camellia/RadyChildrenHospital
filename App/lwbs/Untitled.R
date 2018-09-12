library(shinydashboard)
library(shiny)
library(plotly)
library(tidyverse)

lwbs_rate_df <- read_csv('data/lwbs_rate.csv') %>% select(-X1)
lwbs_time_df <- read_csv('data/lwbs_time.csv') %>% select(-X1)


body <- dashboardBody(
  h3("The LWBS rate trend plot:"),
  dateRangeInput('lwbs_rate_date', 'Select date range: ', start = '2014-01-01', end = '2018-06-07', min = '2014-01-01',max = '2018-06-07',format = "mm/dd/yy"),
  uiOutput('generate'),
  plotOutput('lwbs_rate_plot'),
  h4('Effectiveness of setting physician arrival time threshold:'),
  numericInput('threshold', 'Please enter the threshold in minutes: ', min = 0, value = 90,step = 5),
  uiOutput('analyze'),
  plotOutput('lwbs_reduce'),
  textOutput('lwbs_reduce_text')
)
header <- dashboardHeader()
sidebar <- dashboardSidebar()

ui <- dashboardPage(header, sidebar, body)
server <- function(input, output){
  
      
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
}


shinyApp(ui, server)
