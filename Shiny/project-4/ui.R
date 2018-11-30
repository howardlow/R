library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 4: Howard Low (53626262)"),
  
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             plotOutput("BoxPlots"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max=1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("GLMnet Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("GlmModelSummary1"),
             hr(),
             plotOutput("GlmModelPlots"),
             verbatimTextOutput("GlmModelSummary2")
    ),
    tabPanel("PLS Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("ANN Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("AnnModelSummary1"),
             hr(),
             plotOutput("AnnModelPlots"),
             verbatimTextOutput("AnnModelSummary2")
    ),    
    tabPanel("Random Forest (rf) Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("RfModelSummary1"),
             hr(),
             plotOutput("RfModelPlots"),
             verbatimTextOutput("RfModelSummary2")
    ),      
    tabPanel("Stochastic Gradient Boosting (gbm) Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("GbmModelSummary1"),
             hr(),
             plotOutput("GbmModelPlots"),
             verbatimTextOutput("GbmModelSummary2")
    ), 
    tabPanel("Least Angle Regression (lars2) Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("Lars2ModelSummary1"),
             hr(),
             plotOutput("Lars2ModelPlots"),
             verbatimTextOutput("Lars2ModelSummary2")
    ),     
    tabPanel("Bagged MARS using gCV Pruning (bagEarthGCV) Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("BagEarthModelSummary1"),
             hr(),
             plotOutput("BagEarthyModelPlots"),
             verbatimTextOutput("BagEarthModelSummary2")
    ),    
    tabPanel("Model Selection",
             tags$h3("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             plotOutput("SelectionBoxPlot"),
             radioButtons("Choice", "Model choice", choices = c("GLMnet", "PLS", "ANN", "rf", "gbm", "lar2", "bagEarthGCV"), selected = "bagEarthGCV")
    ),
    
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
  )
))
