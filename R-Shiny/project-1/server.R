#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(Ecdat)

library(shiny)
library(shinyjs)
library(MASS)
library(glmnet)
library(ggplot2)
library(scales)
library(corrplot)
library(dplyr)
library(ggthemes)
library(grid)


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ########################Get Data#################################
  getData <- reactive({
    set.seed(1635)
    data(Fishing)
    Fishing$mode_int <- as.numeric(Fishing$mode)
    Fishing <- Fishing[c(1,13,2,3,4,5,6,7,8,9,10,11,12)]
    Fishing[sample(nrow(Fishing),input$Observations),]
  })
  
  ##################### Data Summary Tab####################################
  
  #------------------------ Data summary ------------------------
  output$DataSummary1 <- renderPrint({
    str(getData())
  })
  
  output$DataSummary2 <- renderPrint({
    summary(getData())
  })

  ##################### Explore Data Tab####################################

  #------------------------ Explore Data ------------------------
  output$Explore <- renderDataTable({
    getData()
  })
  
  
  
  ##################### Model ####################################
  #------------------------ Linear Model ------------------------
  getModel <- reactive({
    d <- getData()
    tryCatch({
        return(lm(formula = as.formula(input$Formula), data=d, x = TRUE))
    }, error = function(e){print(e)})
    return(NULL)
  })
  
  #------------------------ Model Summary ------------------------
  output$ModelSummary <-renderPrint({
    summary(getModel())
  })
  
  
  
  
  ##################### Visualization ####################################  
  #------------------------ Linear Regression ------------------------
  output$LinearRegression <- renderPlot({
    ggplot(getData(), aes_string(x= input$Predictor, y= input$Response)) + 
      geom_point()+
      geom_smooth(method=lm, se=FALSE)
  })
  
  #------------------------ Boxplot ------------------------
  output$Boxplot <- renderPlot({

    fill <- "#4271AE"
    line <- "#1F3552"
    
    ggplot(getData(), aes(x = mode, y = catch)) +
      geom_boxplot(fill = fill, colour = line) +
      
      scale_x_discrete(name = "Fishing Mode")
  })
  
  
  #------------------------ Pie Chart ------------------------
  output$Piechart <- renderPlot({
    
    data <- getData() %>% 
      group_by(mode) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(per=`n`/sum(`n`)) %>% 
      arrange(desc(mode))
    
    data$label <- scales::percent(data$per)
    
    ggplot(data=data)+
      geom_bar(aes(x="", y=per, fill=mode), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)) +
      scale_fill_brewer("Fishing Mode") + blank_theme +
      theme(axis.text.x=element_blank()) 
  })
  #------------------------ Correlation Matrix ------------------------
  
  output$CorMatrix <- renderPlot({
    M <- cor(getData()[sapply(getData(), is.numeric)])
    corrplot(M, method = "circle")
  })
    
  
  
})
