#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(ISLR)
library(shiny)
library(shinyjs)
library(DT)
library(caret)
library(ROCR)
library(ggplot2)
library(plotROC)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  observeEvent(input$TrainDataPercentage,{
    updateSliderInput(session, "TestDataPercentage", value = 100 - input$TrainDataPercentage)
  })

  observeEvent(input$TestDataPercentage,{
    updateSliderInput(session, "TrainDataPercentage", value = 100 - input$TestDataPercentage)
  })
  
  ########################Get Data#################################
  getData <- reactive({
    set.seed(1635)
    data(Carseats)
    Carseats$Urban <- ifelse(Carseats$Urban=='Yes', 1,0)
    Carseats$US <- ifelse(Carseats$US=='Yes', 1,0)
    Carseats$ShelveLoc <- ifelse(Carseats$ShelveLoc=='Good', 3, ifelse(Carseats$ShelveLoc=='Medium', 2, 1))
    d <- Carseats[sample(nrow(Carseats),input$Observations),]
    
    X = subset(d, select = c("Sales", "CompPrice", "Income", "Advertising", "Population", "Price", "ShelveLoc", "Age", "Education"))
    Y = subset(d, select = c("US", "Urban"))
    
    #adding noise
    noise <- matrix(rnorm(input$Observations, mean=1, sd=1), nrow=input$Observations, ncol=9) * input$xNoise
    X2 <- X + noise
    data2 <- cbind(X2, Y)
  })
  
  getTrain <- reactive({
    row.number <- sample(1:nrow(getData()), input$TrainDataPercentage/100*nrow(getData()))
    train = getData()[row.number,]
  })

  getTest <- reactive({
    row.number <- sample(1:nrow(getData()), input$TrainDataPercentage/100*nrow(getData())) 
    test = getData()[-row.number,]
  })
    
  #------------------------ Data summary ------------------------
  output$DataSummary1 <- renderPrint({
    str(getData())
  })
  
  output$DataSummary2 <- renderPrint({
    summary(getData())
  })

  output$DataDescription <- renderPrint({
    c(
    "Sales: Unit sales (in thousands) at each location",
    "CompPrice: Price charged by competitor at each location",
    "Income: Community income level (in thousands of dollars)",
    "Advertising: Local advertising budget for company at each location (in thousands of dollars)",
    "Population: Population size in region (in thousands)",
    "Price: Price company charges for car seats at each site",
    "ShelveLoc: A factor with levels Bad (1), Medium(2) and Good(3) indicating the quality of the shelving location for the car seats at each site",
    "Age: Average age of the local population",
    "Education: Education level at each location",
    "Urban: A factor with levels No (0) and Yes (1) to indicate whether the store is in an urban or rural location",
    "US: A factor with levels No (0) and Yes (1) to indicate whether the store is in the US or not"
    )
  })  
  
  ##################### Explore Data Tab####################################
  ext <- list(Responsive=TRUE, ColReorder=TRUE)
  #------------------------ Explore Data ------------------------
  output$ExploreTrain <- DT::renderDataTable({
    DT::datatable(data=getTrain(), rownames = FALSE,
      options=list(searching = TRUE,
                   pageLength = 5,
                   lengthMenu = c(5, 10, 15, 100)
      ),
      extensions = ext
    )
  })

  output$ExploreTest <- DT::renderDataTable({
    DT::datatable(data=getTest(), rownames = FALSE,
      options=list(searching = TRUE,
                   pageLength = 5,
                   lengthMenu = c(5, 10, 15, 100)
      ),
      extensions = ext
    )
  })
  
  ##################### Model ####################################

  #------------------------ Formulate Formula ------------------------
  getFormula <- reactive({
    varX <- paste(input$Response, "~", sep="")
    varY <- paste(input$Predictors, collapse = "+")
    
    formula <- ""
    if (length(input$Predictors) == 0) {
      formula <- paste(varX, ".", sep="")
    } else {
      forumula <-  paste(varX, varY, sep="")
    }
  })
  
  output$Formula <- renderPrint({
    str(getFormula())
  })
  
  #------------------------ Logistic Model ------------------------
  getModel <- reactive({
    d <- getTrain()
    tryCatch({
      return(glm(formula = as.formula(getFormula()), data=d, family = "binomial"))
    }, error = function(e){print(e)})
    return(NULL)
  })
  
  #------------------------ Model Summary ------------------------
  output$ModelSummary <-renderPrint({
    summary(getModel())
  })
  
  #------------------------ Confusion Matrix ------------------------  
  output$ModelConfusionMatrix <- renderPrint({
    pdata <- predict(getModel(), newdata = getTest(), type = "response")
    testY <- getTest()[,input$Response]
    conf <- confusionMatrix(factor(as.numeric(pdata>0.5)), factor(testY))
    print(conf)
    #print(factor(testY))
    #print(factor(as.numeric(pdata>0.5)))
  })

  #------------------------ AUC ROC  ------------------------  
  output$ROC <- renderPlot({  
    df <- rbind(data.frame(predictor = predict(getModel(), getTrain()),
                           known.truth = getTrain()[,input$Response],
                           model = "train"),
                
                data.frame(predictor = predict(getModel(), getTest()),
                           known.truth = getTest()[,input$Response],
                           model = "test"))
    #print(colnames(df))
    ggplot(df, aes(d = known.truth, m = predictor, color = model)) + 
    geom_roc(n.cuts = 0)
  })
  
  ##################### Visualization ####################################  
  #------------------------ Linear Regression ------------------------
  output$LogisticRegression <- renderPlot({
    ggplot(getTrain(), aes_string(x= input$PredictorX, y= input$ResponseY)) + 
      geom_point()+
      geom_smooth(method = "glm", 
                  method.args = list(family = "binomial"), 
                  se = FALSE) 
  })
  
})
