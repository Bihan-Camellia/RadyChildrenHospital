# install.packages('shinyjs')
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


dashboardSidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("LWBS", tabName = "data", icon = icon("database")),
    menuItem("Model",
      icon = icon("file-text-o"),
      menuSubItem("rch", tabName = "schedule", icon = icon("angle-right")),
      menuSubItem("model", tabName = "linear", icon = icon("angle-right"))
    ),
    # menuItem("Instruction", tabName = "about", icon = icon("info-circle")),
    width = 300
  )
)


dashboardBody <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "about",
      box(
        width = NULL, status = "primary", solidHeader = TRUE, title = "Guildline",
        htmlOutput("rch_about")
      )
    ),
    tabItem(
      tabName = "data",
      fluidRow(
        column(
          width = 10,
          tabBox(
            width = NULL,
            tabPanel(
              h5("General"),
              h3("The LWBS rate trend plot:"),
              dateRangeInput('lwbs_rate_date', 'Select date range: ', start = '2014-01-01', end = '2018-06-07', min = '2014-01-01',max = '2018-06-07',format = "mm/dd/yy"),
              uiOutput('generate'),
              plotOutput('lwbs_rate_plot')
            ),
            tabPanel(
              h5("Explore"),
              h4("Effectiveness of setting physician arrival time threshold:"),
              numericInput('threshold', 'Please enter the threshold in minutes: ', min = 0, value = 90,step = 5),
              uiOutput('analyze'),
              plotOutput('lwbs_reduce'),
              textOutput('lwbs_reduce_text')
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "schedule",
      fluidRow(
        column(
          width = 3,
          box(
            width = NULL, status = "primary", solidHeader = TRUE, title = "",
            "select season and weekday that you want to schedule:",
            br(),
            selectInput("season", "select season:", choices = c("High", "Medium", "Low")),
            selectInput("weekday", "select weekday:", choices = c("Sun/Mon", "Tue-Sat")),
            fileInput("patient_file","Upload", 
                      placeholder = "No file selected", 
                      accept = c('.csv'))
          )
        ),
        column(
          width = 8,
          box(
            width = NULL, status = "primary", solidHeader = TRUE, title = "",
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Acuities 1-2-3",
                numericInput("Pts123", label = "Number of high acuity patients per hour: ", value = 2.15, step = 0.05, min = 0),
                sliderInput("serviceLevelH", label = "What percentile do you want:", value = 0.75, max = 1, min = 0),
                # dataTableOutput('highAcuity'),
                plotOutput("highplot"),
                div(style = "overflow-x: scroll", dataTableOutput("highAcuity"))
              ),
              tabPanel(
                "Acuities 4-5",
                numericInput("Pts45", label = "Number of low acuity patients per hour: ", value = 3.00, step = 0.05, min = 0),
                sliderInput("serviceLevelL", label = "What percentile do you want:", value = 0.75, max = 1, min = 0),
                # dataTableOutput('lowAcuity'),
                plotOutput("lowplot"),
                div(style = "overflow-x: scroll", dataTableOutput("lowAcuity"))
              ),
              tabPanel("Total", plotOutput("totalplot"))
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "linear",
      fluidRow(
        column(
          width = 3,
          box(
            width = NULL, status = "primary", solidHeader = TRUE, title = "",
            uiOutput("select_ui"),
            fileInput("file","Upload", 
                      placeholder = "No file selected", 
                      accept = c('.xlsx')),
            h5("Requirement: The Excel file should contain two variables: Date and Number of Patient, named in ADT_ARRIVAL_DATE, number"),
            br(),
            h5("ADT_ARRIVAL_DATE: Date"),
            h5("number: Daily Patient Volume")
            )
        ),
        column(
          width = 8,
          box(
            width = NULL, status = "primary", solidHeader = TRUE, title = "",
            textOutput("daily_ui"),
            plotOutput("pred_dis", height = 385),
            div(style = "overflow-x: scroll", dataTableOutput("hAcuity")),
            div(style = "overflow-x: scroll", dataTableOutput("lAcuity"))
          )
        )
      )
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "Rady Children's Hospital", titleWidth = 300),
  dashboardSidebar,
  dashboardBody
)
