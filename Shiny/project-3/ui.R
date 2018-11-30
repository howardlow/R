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
library(dplyr)
# Define UI for application that draws a histogram

columnNames <- c('Author','Date','Priority','Price','Speed','Duration','Scarcity','Location','Agreed','State','Class','Surface','Layer1','Layer2','Layer3','Layer4','Layer5','Layer6','Layer7','Layer8','Layer9','Layer10','Layer11','Layer12','Layer13','Layer14','Layer15','Layer16','Layer17','Layer18','Layer19','Layer20','Layer21','Layer22','Layer23','Layer24','Layer25','Layer26','Layer27','Layer28','Layer29','Layer30')
layerColumnNames<- c('Layer1','Layer2','Layer3','Layer4','Layer5','Layer6','Layer7','Layer8','Layer9','Layer10','Layer11','Layer12','Layer13','Layer14','Layer15','Layer16','Layer17','Layer18','Layer19','Layer20','Layer21','Layer22','Layer23','Layer24','Layer25','Layer26','Layer27','Layer28','Layer29','Layer30')

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  
tags$style(".btn {
    background-color: #E8E8E8;
    }
           
    .nav-tabs {
    background-color: #F0F0F0;
    }
           "),
  
  # Application title
  titlePanel("A3: Howard Low (53626262)"),
  tags$br(),
  actionButton("resetBtn", "Reset", icon = icon("retweet")),
  tags$br(),
  tags$br(),  
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Data Structure",
        tags$h3(tags$b("Data Summary:")),         
        plotOutput("Matplot", height = "600px"),        
        verbatimTextOutput("DataSummary1"), 
        tags$br(), 
        verbatimTextOutput("DataSummary2"),
        icon = icon("table")
      ),
      
      
      tabPanel(
        title = "Outliers", 
        icon = icon("search"),        
        tags$h3(tags$b("Outliers Options:")),          
        selectizeInput(
          inputId = 'selectOutliersVariables',
          label = 'Select variables to have their outliers removed:',
          choices = layerColumnNames, 
          selected = c('Layer2', 'Layer5', 'Layer13', 'Layer15'),
          multiple = TRUE,
          width = '100%',
          options = list(
            render = I("{item: function(item, escape) {return '<div class=\"item\" onclick=\"Shiny.onInputChange(\\\'select_click\\\', \\\'' + escape(item.value) + '\\\')\">' + escape(item.value) + '</div>';}}")
          )
        ),
        actionButton("removeOutliersBtn", "Remove", icon=icon("trash")),
        tags$br(),        
        tags$br(),        
        tags$br(),        
        tabsetPanel(
          tabPanel("IQR", 
            icon = icon("signal"),
            tags$br(),                    
            sliderInput("mMultiplier", label = "IQR multiplier", min = 0, max = 4, value = 1.5, step=0.1),                   
            fluidRow(
              column(3, plotOutput("mBox1")),
              column(3, plotOutput("mBox2")),
              column(3, plotOutput("mBox3")),
              column(3, plotOutput("mBox4"))
            ),
            fluidRow(
              column(3, plotOutput("mBox5")),
              column(3, plotOutput("mBox6")),
              column(3, plotOutput("mBox7")),
              column(3, plotOutput("mBox8"))
            ),
            fluidRow(
              column(3, plotOutput("mBox9")),
              column(3, plotOutput("mBox10")),
              column(3, plotOutput("mBox11")),
              column(3, plotOutput("mBox12"))
            ),
            fluidRow(
              column(3, plotOutput("mBox13")),
              column(3, plotOutput("mBox14")),
              column(3, plotOutput("mBox15")),
              column(3, plotOutput("mBox16"))
            ),
            fluidRow(
              column(3, plotOutput("mBox17")),
              column(3, plotOutput("mBox18")),
              column(3, plotOutput("mBox19")),
              column(3, plotOutput("mBox20"))
            )
          ),
          tabPanel("Cook",
            icon = icon("signal"),                   
            tags$br(),                   
            sliderInput("mFactor", label = "Factor", min = 0, max = 12, value = 4, step=0.1),
            plotOutput("mCooks"),
            tableOutput("mObs")                   
          )
        ),
        tags$br()
      ),
      
      
      tabPanel(
        title = "Missing Data",
        icon = icon("eraser"),                
        tags$h3(tags$b("Missing Data Options:")),            
        radioButtons("cleanMissingChoice", "Select an option to handle missing values:", inline = TRUE,
                     c("Remove" = "Remove",
                       "Impute (Not recommended)" = "Impute")),
        hidden(
          selectizeInput(
            inputId = 'selectMissingValueVariables',
            label = 'Select variables to have their missing values removed:',
            choices = columnNames, 
            selected = layerColumnNames,
            multiple = TRUE,
            width = '100%',
            options = list(
              render = I("{item: function(item, escape) {return '<div class=\"item\" onclick=\"Shiny.onInputChange(\\\'select_click\\\', \\\'' + escape(item.value) + '\\\')\">' + escape(item.value) + '</div>';}}")
            )
          )
        ),
        hidden(
          actionButton("removeMissingValuesBtn", "Remove", icon=icon("trash"))
        ),  
        hidden(
          actionButton("imputeMissingValuesBtn", "Impute", icon=icon("pencil"))
        ),
        tags$br(),
        tags$br(),        
        plotOutput("VisMissPlot", height = "700px"),
        tags$br(),
        plotOutput("MissingDataPattern", height = "500px"),
        tags$br()
      ),
      
      
      tabPanel(
        title = "Data Split",
        icon = icon("random"),           
        tags$h3(tags$b("Training/Testing Split:")),         
        sliderInput("TrainDataPercentage", "Training Data (%)", min = 0, max = 100, step = 1, value = 80, width="100%"),
        tags$h3(tags$b("Training Records:")), 
        dataTableOutput("ExploreTrain"),
        tags$br(),          
        tags$h3(tags$b("Testing Records:")),
        dataTableOutput("ExploreTest")
      ),
      
      
      tabPanel(
        title = "Dim Reduce",
        icon = icon("compress"),          
        tags$h3(tags$b("PCA:")),           
        sliderInput("Components", "Components:", min = 0, max = 20, value = 3, width="100%"),
        tags$em(textOutput("dimReduceNote")),
        tags$h3(tags$b("Scree Plot:")),
        plotOutput("ScreePlot", height = "700px")
      ),
      
      tabPanel(
        title = "Train",
        icon = icon("wrench"),          
        tags$h3(tags$b("Linear Model:")),          
        selectizeInput(
          inputId = 'selectPredictors',
          label = 'Select variables to build your linear model:',
          choices = columnNames, 
          selected = c('Priority', 'Price', 'Speed', 'Duration', 'Location', 'State', 'Class', 'Surface', 'Layer6', 'Layer8', 'Layer10', 'Layer18'),
          multiple = TRUE,
          width = '100%',
          options = list(
            render = I("{item: function(item, escape) {return '<div class=\"item\" onclick=\"Shiny.onInputChange(\\\'select_click\\\', \\\'' + escape(item.value) + '\\\')\">' + escape(item.value) + '</div>';}}")
          )
        ),
        tags$br(), 
        tags$h3(tags$b("Formula:")), 
        verbatimTextOutput("Formula"),
        tags$br(), 
        tags$h3(tags$b("Model Summary:")), 
        verbatimTextOutput("ModelSummary"),
        tags$br(), 
        tags$h3(tags$b("Residual Plot:")), 
        plotOutput("ResidualPlot", height = "700px")
      ),
      
      tabPanel(
        title = "Test",
        icon = icon("question"),          
        tags$h3(tags$b("Performance Statistic:")), 
        verbatimTextOutput("performanceStatistic"),
        tags$br(), 
        tags$h3(tags$b("Predicted vs Actual Plot:")), 
        plotOutput("PredictedActualPlot", height = "700px")
      )
    ),
    width = 12)
))
