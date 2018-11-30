#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(mice)
library(VIM)
library(ggplot2)
library(naniar)
library(shinyjs)
library(DT)

columnNames <- c('Y','Author','Date','Priority','Price','Speed','Duration','Scarcity','Location','Agreed','State','Class','Surface','Layer1','Layer2','Layer3','Layer4','Layer5','Layer6','Layer7','Layer8','Layer9','Layer10','Layer11','Layer12','Layer13','Layer14','Layer15','Layer16','Layer17','Layer18','Layer19','Layer20','Layer21','Layer22','Layer23','Layer24','Layer25','Layer26','Layer27','Layer28','Layer29','Layer30')
layerColumnNames<- c('Layer1','Layer2','Layer3','Layer4','Layer5','Layer6','Layer7','Layer8','Layer9','Layer10','Layer11','Layer12','Layer13','Layer14','Layer15','Layer16','Layer17','Layer18','Layer19','Layer20','Layer21','Layer22','Layer23','Layer24','Layer25','Layer26','Layer27','Layer28','Layer29','Layer30')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  initFlag <- TRUE
  ########################Get Data#################################
  getData <- function() {
    set.seed(1635)
    data <- read.csv("Ass3Data.csv") 

    #set ID as rowname
    row.names(data) <- data$ID
    data$ID <- NULL
    
    #sort factor ordering for Priority
    #"Low"    "Medium" "High"  
    data$Priority <- factor(data$Priority,levels(data$Priority)[c(2,3,1)])
    
    #sort factor ordering for Speed
    #"Slow"   "Medium" "Fast"
    data$Speed <- factor(data$Speed,levels(data$Speed)[c(3,2,1)])
    
    #sort factor ordering for Duration
    #"Short"     "Long"      "Perpetual"
    data$Duration <- factor(data$Duration,levels(data$Duration)[c(3,1,2)])
    
    #sort factor ordering for Scarcity
    #"Common"  "Typical" "Rare" 
    data$Scarcity <- factor(data$Scarcity,levels(data$Scarcity)[c(1,3,2)])

    #sort factor ordering for State
    #"Uncertain"    "Under review" "Checked"
    data$State <- factor(data$State,levels(data$State)[c(2,3,1)])
    
    #sort factor ordering for Surface
    #"Smooth"   "Textured" "Rough"
    data$Surface <- factor(data$Surface,levels(data$Surface)[c(2,3,1)])
    
    #convert String to date
    data$Date <- as.Date(data$Date)
    
    return(data)
  }

  #initalize reactive values
  RA <- reactiveValues(data = getData())
  
  ##################### Reset ####################################  
  #------------------------ Reset button ------------------------  
  observeEvent(input$resetBtn, {
    RA$data <- getData()
    initFlag <<- TRUE
    populateModelVariables()
  })
  
  ##################### Data Structure ####################################  
  output$DataSummary1 <- renderPrint({
    str(RA$data)
  })
  
  output$DataSummary2 <- renderPrint({
    summary(RA$data)
  })

  output$ColumnNames <- renderText({
    colnames(RA$data)
  })  
  
  #------------------------ Matplot to check Structural outliers ------------------------  
  output$Matplot <- renderPlot({
    matplot(y = RA$data, type = "l", xlab = "obs", ylab="Variables", lty = 1:43, lwd=1)
    legend(legend = colnames(RA$data), x = "topright", y = "topright", lty = 1:43, lwd = 1, ncol=4, col=1:43)
  })
  
  
  ##################### Outliers ####################################  
  #------------------------ Remove outliers button ------------------------  
  observeEvent(input$removeOutliersBtn, {
    clist <- input$selectOutliersVariables
    for (i in clist) {
      outliers <- boxplot.stats(RA$data[,i], coef = input$mMultiplier, do.conf = TRUE, do.out = TRUE)$out
      RA$data <- RA$data[ !RA$data[,i] %in% outliers, ]
    }
  })

  #------------------------ Boxplot ------------------------  
  output$mBox1 <- renderPlot({
    df <- RA$data
    hist(df$Layer1 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer1, boxwex=6, main="Layer1", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox2 <- renderPlot({
    df <- RA$data
    hist(df$Layer2 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer2, boxwex=6, main="Layer2", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox3 <- renderPlot({
    df <- RA$data
    hist(df$Layer3 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer3, boxwex=6, main="Layer3", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox4 <- renderPlot({
    df <- RA$data
    hist(df$Layer4 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer4, boxwex=6, main="Layer4", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox5 <- renderPlot({
    df <- RA$data
    hist(df$Layer5 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer5, boxwex=6, main="Layer5", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox6 <- renderPlot({
    df <- RA$data
    hist(df$Layer6 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer6, boxwex=6, main="Layer6", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })   
  
  output$mBox7 <- renderPlot({
    df <- RA$data
    hist(df$Layer7 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer7, boxwex=6, main="Layer7", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox8 <- renderPlot({
    df <- RA$data
    hist(df$Layer8 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer8, boxwex=6, main="Layer8", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox9 <- renderPlot({
    df <- RA$data
    hist(df$Layer9 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer9, boxwex=6, main="Layer9", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox10 <- renderPlot({
    df <- RA$data
    hist(df$Layer10 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer10, boxwex=6, main="Layer10", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox11 <- renderPlot({
    df <- RA$data
    hist(df$Layer11 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer11, boxwex=6, main="Layer11", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox12 <- renderPlot({
    df <- RA$data
    hist(df$Layer12 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer12, boxwex=6, main="Layer12", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox13 <- renderPlot({
    df <- RA$data
    hist(df$Layer13 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer13, boxwex=6, main="Layer13", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox14 <- renderPlot({
    df <- RA$data
    hist(df$Layer14 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer14, boxwex=6, main="Layer14", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox15 <- renderPlot({
    df <- RA$data
    hist(df$Layer15 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer15, boxwex=6, main="Layer15", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })
  
  output$mBox16 <- renderPlot({
    df <- RA$data
    hist(df$Layer16 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer16, boxwex=6, main="Layer16", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })
  
  output$mBox17 <- renderPlot({
    df <- RA$data
    hist(df$Layer17 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer17, boxwex=6, main="Layer17", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })
  
  output$mBox18 <- renderPlot({
    df <- RA$data
    hist(df$Layer18 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer18, boxwex=6, main="Layer18", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox19 <- renderPlot({
    df <- RA$data
    hist(df$Layer19 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer19, boxwex=6, main="Layer19", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox20 <- renderPlot({
    df <- RA$data
    hist(df$Layer20 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer20, boxwex=6, main="Layer20", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox21 <- renderPlot({
    df <- RA$data
    hist(df$Layer21 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer21, boxwex=6, main="Layer21", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox22 <- renderPlot({
    df <- RA$data
    hist(df$Layer22 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer22, boxwex=6, main="Layer22", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox23 <- renderPlot({
    df <- RA$data
    hist(df$Layer23 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer23, boxwex=6, main="Layer23", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox24 <- renderPlot({
    df <- RA$data
    hist(df$Layer24 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer24, boxwex=6, main="Layer24", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox25 <- renderPlot({
    df <- RA$data
    hist(df$Layer25 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer25, boxwex=6, main="Layer25", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox26 <- renderPlot({
    df <- RA$data
    hist(df$Layer26 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer26, boxwex=6, main="Layer26", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox27 <- renderPlot({
    df <- RA$data
    hist(df$Layer27 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer27, boxwex=6, main="Layer27", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox28 <- renderPlot({
    df <- RA$data
    hist(df$Layer28 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer28, boxwex=6, main="Layer28", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox29 <- renderPlot({
    df <- RA$data
    hist(df$Layer29 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer18, boxwex=6, main="Layer29", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })  
  
  output$mBox30 <- renderPlot({
    df <- RA$data
    hist(df$Layer30 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
    boxplot(df$Layer30, boxwex=6, main="Layer30", range=input$mMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
  })    

  #------------------------ Cooks ------------------------    
  getmCooks <- reactive({
    df <- RA$data
    mod <- lm(formula = Y ~., data=df)
    cooks.distance(mod)
  })
  
  output$mCooks <- renderPlot({
    fac <- input$mFactor
    cooksd <- data.frame(cooksd=getmCooks())
    df <- RA$data
    ggplot(cooksd, aes(x=seq_along(cooksd), y=cooksd))+
      geom_bar(stat="identity", position="identity") +
      xlab("Obs. Number")+
      ylab("Cook's distance") +
      ggtitle("Cook's distance")+
      geom_hline(yintercept=fac*mean(cooksd$cooksd, na.rm=T)) +
      theme(legend.position="none")
  })

  ##################### Missing Values ####################################  
  observeEvent(input$cleanMissingChoice, {
    
    if (input$cleanMissingChoice == 'Impute') {
        toggle("imputeMissingValuesBtn")
        hide("removeMissingValuesBtn")
        hide("selectMissingValueVariables")
    } else {
      toggle("removeMissingValuesBtn")
      toggle("selectMissingValueVariables")
      hide("imputeMissingValuesBtn")
    }
    
  })
  #------------------------ Remove Missing value button ------------------------  
  observeEvent(input$removeMissingValuesBtn, {
    clist <- input$selectMissingValueVariables
    for (i in clist) {
      RA$data <- RA$data[ !is.na(RA$data[,i]), ]
    }
  })  
  
  #------------------------ Impute Missing value button ------------------------  
    observeEvent(input$imputeMissingValuesBtn, {
      withProgress(message = 'Imputing', value = 0, {
        init = mice(RA$data, maxit=0) 
        meth = init$method
        incProgress(1/4)
        predM = init$predictorMatrix
        set.seed(103)
        predM[, c("Author")]=0
        meth[c('Layer1','Layer2','Layer3','Layer4','Layer5','Layer6','Layer7','Layer8','Layer9','Layer10','Layer11','Layer12','Layer13','Layer14','Layer15','Layer16','Layer17','Layer18','Layer19','Layer20','Layer21','Layer22','Layer23','Layer24','Layer25','Layer26','Layer27','Layer28','Layer29','Layer30')]="norm" 
        meth[c("Agreed")]="logreg" 
        meth[c('Priority','Price','Speed','Duration','Scarcity','Location','State','Class','Surface')]="polyreg"
    
        imputed = mice(RA$data, method=meth, predictorMatrix=predM, m=5)
        incProgress(2/4)
        imputed <- complete(imputed)
        incProgress(3/4)
        RA$data <- imputed
        incProgress(4/4)
      })
  })
  
  #------------------------ Missing Data Pattern ------------------------
  output$MissingDataPattern <- renderPlot({
    VIM::aggr(RA$data, col=c('navyblue','yellow'),
              numbers=TRUE, sortVars=TRUE,
              labels=names(getData()), cex.axis=.7,
              gap=3, ylab=c("Histogram of missing data","Pattern"))
    
  })

  #------------------------ Vis Miss Plot ------------------------  
  output$VisMissPlot <- renderPlot({
    vis_miss(RA$data)
  })

  ##################### Data Split ####################################  
  getTrain <- reactive({
    row.number <- sample(1:nrow(RA$data), input$TrainDataPercentage/100*nrow(RA$data))
    train =RA$data[row.number,]
  })
  
  getTest <- reactive({
    row.number <- sample(1:nrow(RA$data), input$TrainDataPercentage/100*nrow(RA$data)) 
    test = RA$data[-row.number,]
  })
  
  #------------------------ Explore Data ------------------------
  ext <- list(Responsive=TRUE, ColReorder=TRUE)
  output$ExploreTrain <- DT::renderDataTable({
    DT::datatable(data=getTrain(), rownames = TRUE,
                  options=list(searching = TRUE,
                               pageLength = 5,
                               lengthMenu = c(5, 10, 15, 100)
                  ),
                  extensions = ext
    )
  })
  
  output$ExploreTest <- DT::renderDataTable({
    DT::datatable(data=getTest(), rownames = TRUE,
                  options=list(searching = TRUE,
                               pageLength = 5,
                               lengthMenu = c(5, 10, 15, 100)
                  ),
                  extensions = ext
    )
  })
  
  ##################### Dim Reduce ####################################  
  output$dimReduceNote <- renderText({
    "*Please note that Dim Reduce is only performed for 'Layer1' to 'Layer30'"
  })
  
  getRegModel <- reactive({
    train.data <- getTrain()
    
    numeric.train <- sapply(train.data, is.numeric)
    numData.train <- train.data[,numeric.train]
    numData.train <- numData.train[,-grep("Y",colnames(numData.train))]
    
    prin_comp <- prcomp(numData.train, center = TRUE, scale = TRUE)
  })

  output$ScreePlot <- renderPlot({
    tryCatch({
      prin_comp <- getRegModel()
      pca_no <- input$Components
      
      std_dev <- prin_comp$sdev
      pr_var <- (std_dev[1:pca_no])^2
      prop_varex <- pr_var/sum(pr_var)
      
      plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of X Variance Explained", type = "b", ylim=c(0,1), col="green") +
        lines(cumsum(prop_varex), col="blue", type="b")
      legend("right", c("Accummulation","Contribution"), text.col = c("blue","green"))
      
      if (initFlag == TRUE) {
        updateSliderInput(session, "Components", value = 3) #3 is the optimal
        initFlag <<- FALSE        
      }
      
    }, error = function(err) {
      prin_comp <- ""
      updateSliderInput(session, "Components", value = 0)
      validate(
        need(prin_comp != "", "Unable to perform Dim Reduce as there is missing value(s) in the dataset, please go to [Missing value] tab to decide an option to handle the missing record(s)."
        )
      )
      
    }, finally = {
      populateModelVariables()
    })
  })
  
  getTrainPca <- reactive({
    train_pca <- cbind(getTrain()[1:13], getRegModel()$x)
  })

  ##################### Train ####################################
  #------------------------ Populate Model Variables ------------------------
  
  populateModelVariables <- reactive ({
    if ((initFlag == TRUE) | (input$Components == 0)) {
      updateSelectInput(session, "selectPredictors",
                        choices = colnames(getTrain()[,-1]),
                        selected = c('Priority', 'Price', 'Speed', 'Duration', 'Location', 'State', 'Class', 'Surface', 'Layer6', 'Layer8', 'Layer10', 'Layer18')
      )
    } else {
      updateSelectInput(session, "selectPredictors",
                        choices = colnames(getTrainPca()[,-1]),
                        selected = c('Priority', 'Price', 'Speed', 'Duration', 'Location', 'State', 'Class', 'Surface', 'PC1', 'PC2', 'PC3')
      )  
    }
  })
  
  #------------------------ Formulate Formula ------------------------  
  getFormula <- reactive({
    varY <- paste("Y", "~", sep="")
    varX <- paste(input$selectPredictors, collapse = "+")
    
    formula <- ""
    if (length(input$selectPredictors) == 0) {
      formula <- paste(varY, ".", sep="")
    } else {
      forumula <-  paste(varY, varX, sep="")
    }
  })
  
  output$Formula <- renderPrint({
    str(getFormula())
  })

  #------------------------ Linear Model ------------------------    
  getModel <- reactive({
    d <- getTrain()
    if ((initFlag == FALSE) & (input$Components > 0)) {
      d <- getTrainPca()
    }
    
    tryCatch({
      return(lm(formula = as.formula(getFormula()), data=d, x=TRUE))
    }, error = function(e){print(e)})
    return(NULL)
  })
  
  #------------------------ Model Summary ------------------------
  output$ModelSummary <-renderPrint({
    summary(getModel())
  })
  
  
  output$ResidualPlot <- renderPlot({
    fit <- getModel()
    fit$fitted.values
    plot(fit$fitted.values,fit$residuals,pch=16,col='red')
    abline(h=0,col="blue")
  })
  
  ##################### Test ####################################
  getTestPca <- reactive({
    test.data <- getTest()
    
    numeric.test <- sapply(test.data, is.numeric)
    numData.test <- test.data[,numeric.test]
    numData.test <- numData.test[,-grep("Y",colnames(numData.test))]
    
    prcomp <- getRegModel()
    
    test_pca <- predict(prcomp, newdata = numData.test)
    new_test <- cbind(getTest()[1:13], test_pca)
  })
  
  output$PredictedActualPlot <- renderPlot({
    d <- getTest()
    if ((initFlag == FALSE) & (input$Components > 0)) {
      d <- getTestPca()
    }

    pred <- predict(getModel(), newdata = d)
    mse <- sum((pred - d$Y)^2, na.rm=T)/length(d$Y)
    
    plot(pred, d$Y, xlab="predicted",ylab="actual")
    abline(a=0,b=1)
    text(20, max(pred, na.rm=T)/2, paste("MSE: ", mse), cex = 1.5)
  })
  
  output$performanceStatistic <- renderText({

    mod <- getModel()
    mod.matrix <- mod$x
    
    obs <- dim(mod.matrix)[1]
    numTrain <- nrow(getTrain())
    numTest <- nrow(getTest())

    numParams <- dim(mod.matrix)[2]
    numAfterClean <- nrow(RA$data)
    numBeforeClean <- nrow(getData())
    numRecordsClean <- numBeforeClean - numAfterClean

    vector1 <- c('No. of Training Obs:','No. of Testing Obs:','No. of Removed Obs:', 'No. of Parameters Used:')
    vector2 <- c(numTrain, numTest, numRecordsClean, numParams)
    array1 <- array(c(vector1,vector2), dim = c(4,2))
    paste(" No. of Training Obs:" ,numTrain, "\n",
          "No. of Testing Obs:" ,numTest, "\n",
          "No. of Removed Obs:" ,numRecordsClean, "\n",
          "No. of Parameters Used:" ,numParams
          )

  })

})
