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
library(DT)
library(ISLR)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("A2: Howard Low (53626262), Sales of Child Car Seats"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=3,
      sliderInput("Observations", "Obervations", min = 10, max = 400, step = 10, value = 400),
      sliderInput("TrainDataPercentage", "Training Data (%)", min = 0, max = 100, step = 1, value = 80),
      sliderInput("TestDataPercentage", "Testing Data (%)", min = 0, max = 100, step = 1, value = 20),
      sliderInput("xNoise", "Scale of X noise:", min = 0, max = 20, value = 0, step = 1)      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data Summary", 
          tags$h3(tags$b("Data Description:")), 
          verbatimTextOutput("DataDescription"), 
          tags$br(), 
          tags$h3(tags$b("Data Summary:")), 
          verbatimTextOutput("DataSummary1"), 
          tags$br(), 
          verbatimTextOutput("DataSummary2")
        ),
        tabPanel(
          title = "Explore Data", 
          tags$h3(tags$b("Training Records:")), 
          dataTableOutput("ExploreTrain"),
          tags$br(),          
          tags$h3(tags$b("Testing Records:")), 
          dataTableOutput("ExploreTest")
        ),
        tabPanel(
          title = "Logistic Model",
          tags$h3(tags$b("Formula Input:")), 
          fluidRow(
            column(4,
                   selectInput("Response",
                               h3("Response (Y):"),
                               selected ="",
                               c("US", "Urban")
                   )
            )
          ),
          checkboxGroupInput("Predictors", label = h3("Predictor (X)"),  inline = TRUE,
                             choices = list(
                                            "Sales" = "Sales", 
                                            "CompPrice" = "CompPrice",
                                            "Income" = "Income",
                                            "Advertising" = "Advertising",
                                            "Population" = "Population",
                                            "Price" = "Price",
                                            "ShelveLoc" = "ShelveLoc",
                                            "Age" = "Age",
                                            "Education" = "Education"
                                            ),
                             selected = c("Income","Advertising", "Population")),
          tags$br(), 
          tags$h3(tags$b("Formula:")), 
          verbatimTextOutput("Formula"),          
          tags$br(), 
          tags$h3(tags$b("Model Summary:")), 
          verbatimTextOutput("ModelSummary"),
          tags$br(), 
          tags$h3(tags$b("Confusion Matrix (Test data):")), 
          verbatimTextOutput("ModelConfusionMatrix"),
          tags$br(), 
          tags$h3(tags$b("ROC Curve:")), 
          #verbatimTextOutput("ROC")
          plotOutput("ROC")
        ),
        tabPanel(
          title = "Logistic Regression",
          tags$h3(tags$b("Logistic Regression between variables:")),           
          fluidRow(
            column(4,
                   selectInput("ResponseY",
                               h3("Response (Y):"),
                               selected ="US",
                               c("US", "Urban")
                   )
            ),
            column(4,
                   selectInput("PredictorX",
                               h3("Predictor (X):"),
                               selected ="Advertising",
                               c("Sales", "CompPrice", "Income", "Advertising", "Population", "Price", "ShelveLoc", "Age", "Education")
                   )
            )
          ),
          plotOutput("LogisticRegression"),
          tags$br()
        )
      )
    )
  )
))
