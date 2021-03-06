library(shiny)
library(DT)
library(caret)
library(doParallel)
library(pls)
library(nnet)
library(glmnet)
library(lars)
library(gbm)
library(randomForest)
library(earth)

clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(clus)  # this will work on windows
trControl <- trainControl("cv", number = 20)  # shared cross validation specification

shinyServer(function(input, output, session) {

  getData <- reactive({
    data <- read.csv(file="Ass4Data.csv")
    rownames(data) <- data$ID
    data$ID <- NULL
    data
  })
  
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    boxplot(d[,numeric], outline=TRUE, main="Boxplot using multiplier of 1.5")
  })
  
  output$DataSummary <- renderPrint({
    str(getData())
  })

  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  
  ###########################Test Train Split ######################################### 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })

  ############################## Random Forest (RF) ######################################
  getRfModels <- reactive({
    method <- "rf"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(.mtry =  seq(1, 20, by = 1)), #we have 20 predictors so we test out 1 to 20
                  allowParallel = TRUE
    ) 
    removeNotification(id=method)
    mods
  })

  output$RfModelSummary1 <- renderTable({
    mods <- getRfModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$RfModelPlots <- renderPlot({
    plot(getRfModels())
  })     
  
  output$RfModelSummary2 <- renderPrint({
    mods <- getRfModels()
    print(mods$finalModel)
  })

  ##############################  Stochastic Gradient Boosting (GBM) ######################################
  getGbmModels <- reactive({
    method <- "gbm"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(n.trees = seq(50,3000,50), interaction.depth = seq(6, 10, by = 1), shrinkage = c(0.1, 0.01), n.minobsinnode = c(1))
                  #encounter error when running on Parallel
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$GbmModelSummary1 <- renderTable({
    mods <- getGbmModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$GbmModelPlots <- renderPlot({
    plot(getGbmModels())
  })     
  
  output$GbmModelSummary2 <- renderPrint({
    mods <- getGbmModels()
    print(mods$finalModel)
  })

  ##############################  Least Angle Regression (lars2) ######################################
  getLars2Models <- reactive({
    method <- "lars2"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(step = seq(1, 20, by = 1))       
                  #allowParallel = TRUE  #encounter error when running on paraell                
    )
    removeNotification(id=method)
    mods
  })
  
  output$Lars2ModelSummary1 <- renderTable({
    mods <- getLars2Models()
    as.data.frame(mods$bestTune)
  })  
  
  output$Lars2ModelPlots <- renderPlot({
    plot(getLars2Models())
  })     
  
  output$Lars2ModelSummary2 <- renderPrint({
    mods <- getLars2Models()
    print(mods$finalModel)
  })  
  
  
  
  ##############################  Bagged MARS using gCV Pruning (bagEarthGCV) ######################################
  getBagEarthModels <- reactive({
    method <- "bagEarthGCV"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(degree=c(2,3,4))                  
                  #allowParallel = TRUE  #encounter error when running on paraell                
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$BagEarthModelSummary1 <- renderTable({
    mods <- getBagEarthModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$BagEarthModelPlots <- renderPlot({
    plot(getBagEarthModels())
  })     
  
  output$BagEarthModelSummary2 <- renderPrint({
    mods <- getBagEarthModels()
    print(mods$finalModel)
  })  
  
  ############################### GLM ###############################################  
  getGlmModels <- reactive({
    method <- "glmnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(alpha = seq(0,1, 0.1), lambda = seq(0.1, 10, by = 0.1))
    ) # note glmnet does not support parameter "allowParallel"
    removeNotification(id=method)
    mods
  })
  
  output$GlmModelSummary1 <- renderTable({
    mods <- getGlmModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$GlmModelPlots <- renderPlot({
    mods <- getGlmModels()
    plot(mods$finalModel)
  })     
  
  output$GlmModelSummary2 <- renderPrint({
    print(getGlmModels())
  })
  
  
  ############################## PLS ################################################
  getPlsModels <- reactive({
    method <- "pls"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", scale = FALSE, 
                  trControl = trControl,
                  tuneGrid = expand.grid(ncomp = seq(1, 10, by = 1)),
                  allowParallel = TRUE
                  )
    
    removeNotification(id=method)
    mods
  })
  
  output$PlsModelSummary1 <- renderTable({
    mods <- getPlsModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModels())
  })     
  
  output$PlsModelSummary2 <- renderPrint({
    mods <- getPlsModels()
    summary(mods$finalModel)
  })
  
  
  
  ############################### ANN ###############################################
  getAnnModels <- reactive({
    method <- "nnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", maxit = 1000, trace = F, linout = 1,
                  trControl = trControl,
                  tuneGrid = expand.grid(.decay = seq(0.3, 0.6, by=0.1), .size = seq(4, 8, by=1)),
                  allowParallel = TRUE
                  ) 
    removeNotification(id=method)
    mods
  })
  
  output$AnnModelSummary1 <- renderTable({
    mods <- getAnnModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$AnnModelPlots <- renderPlot({
    plot(getAnnModels())
  })     
  
  output$AnnModelSummary2 <- renderPrint({
    mods <- getAnnModels()
    print(mods$finalModel)
  })
  
  ###############################Model Selection######################################  
  getAllModels <- reactive({
    list(GLMnet=getGlmModels(), PLS=getPlsModels(), ANN=getAnnModels(), rf=getRfModels(), gbm=getGbmModels(), lar2= getLars2Models(), bagEarthGCV=getBagEarthModels())  # expand this list with further models
  })
  
  output$SelectionSummary <- renderPrint({
    results <- resamples(getAllModels())
    summary(results)
  })
  
  output$SelectionBoxPlot <- renderPlot({
    results <- caret::resamples(getAllModels())
    bwplot(results, notch=input$Notch)
  })
  
  
  ################################ Test ###################################### 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })

  getTestResults <- reactive({
    test <- getTestData()
    mod <- getAllModels()[input$Choice]
    predictions <- predict(mod, newdata=test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })

  output$TestPlot <- renderPlot({
    plot(getTestResults(), main="Predicted versus Observed")
    abline(a = 0, b=1)
  })
  
})
