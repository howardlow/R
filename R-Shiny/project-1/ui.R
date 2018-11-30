#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(Ecdat)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs(),

  # Application title  
  titlePanel("A1: Howard Low (53626262), Choice of Fishing Mode"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(width=3,
      sliderInput("Observations", "Obervations", min = 10, max = 1182, step = 10, value = 1182)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
            title = "Data Summary", 
            tags$h3(tags$b("Data Summary:")), 
            verbatimTextOutput("DataSummary1"), 
            tags$br(), 
            verbatimTextOutput("DataSummary2")
        ),
        tabPanel(
          title = "Explore Data", 
          tags$h3(tags$b("Records:")), 
          dataTableOutput("Explore")
        ),
        tabPanel(
          title = "Linear Model",
          tags$h3(tags$b("Formula Input:")), 
          textAreaInput("Formula", label = "", value = "catch ~.  -pbeach - ppier -cbeach -cpier -cboat -income -mode_int",rows = 3, width='100%'),
          tags$br(), 
          tags$h3(tags$b("Model Summary:")), 
          verbatimTextOutput("ModelSummary")
        ),
        tabPanel(
          title = "Visualization",
          tags$h3(tags$b("Linear Regression between variables:")),           
          fluidRow(
            column(6,
                   selectInput("Response",
                               "Response (Y):",
                               selected ="",
                               c("catch",
                                 unique(as.character(colnames(Fishing)))))),
            column(6,
                   selectInput("Predictor",
                               "Predictor (X):",
                               selected = "",                               
                               c("ccharter",
                                 unique(as.character(colnames(Fishing))))))
          ),
          plotOutput("LinearRegression"),
          tags$br(),            
          tags$h3(tags$b("Boxplot of catch rate by fishing mode:")), 
          plotOutput("Boxplot"),
          tags$br(),           
          tags$h3(tags$b("Pie Chart of fishing mode:")), 
          plotOutput("Piechart"),
          tags$br(),           
          tags$h3(tags$b("Correlation Matrix between variables:")), 
          plotOutput("CorMatrix")
        )
      )
    )

  )
))
