library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Load general plot theme and colors for color brewer
  source("../plottheme/styling.R",local = TRUE)
  
  N <- 100 #population size
  mean <- 3.5 # population before mean
  sd <- 1.2 # population sd 
  n <- 5 # small sample size
  
  # container for samples
    samples <- reactiveValues(
    lastsample = as.numeric(),
    singlesamplehist = as.numeric(),
    meanhist = as.numeric(),
    counter = 0
  )
  #Make a before and after population
  before <- rnorm(n = N, mean = mean, sd =sd)
  after <- before - runif(n = N,0,2)
  population <- data.frame(before = before, after = after)
  
  #Smallsamplebuttonpress
  observeEvent(input$smallsamplebutton,{
    samples$counter <<- samples$counter + 1
    if(samples$counter == 6) {
      samples$counter <<- 1
      samples$singlesamplehist <<- numeric()
    }
    samples$lastsample <<- sample(1:N,1)
    difference <- before[samples$lastsample] - after[samples$lastsample]
    if(samples$counter <= n){
      samples$singlesamplehist <<- c(samples$singlesamplehist,difference)
    }
    if(samples$counter == n){
      samples$meanhist <<- c(samples$meanhist,mean(samples$singlesamplehist))
    }
    # Limit size of sample to 3 Mb
    if(object.size(samples) > 3e+06) {
      samples$lastsample <<- as.numeric()
      samples$meanhist <<- as.numeric()
      samples$singlesamplehist <<- as.numeric()
      samples$counter <<- 0
    }
  }
  )
  #Largesamplebuttonpress
  observeEvent(input$largesamplebutton,{
    largesample <- sample(1:N,1000,replace = TRUE)
    difference <- before[largesample] - after[largesample]
    samples$lastsample <<- largesample[length(largesample)]
    largesamplemeans <- as.numeric(by(difference,INDICES = as.factor(rep(1:(1000/n),each = n)), FUN = function(x) mean(x)))
    samples$meanhist <<- c(samples$hist, largesamplemeans) 
    # Limit size of sample to 3 Mb
    if(object.size(samples) > 3e+06) {
      samples$lastsample <<- as.numeric()
      samples$meanhist <<- as.numeric()
      samples$singlesamplehist <<- as.numeric()
      samples$counter <<- 0
    }
  }
  )
  #Resetbuttonpress
  observeEvent(input$resetbutton,{
    samples$lastsample <<- as.numeric()
    samples$meanhist <<- as.numeric()
    samples$singlesamplehist <<- as.numeric()
    samples$counter <<- 0
  }
  )
  
  #Before population plot
  output$beforepopplot <- renderPlot({
    
    samplefill <- rep(NA,N)
    
    samplefill[which(sort(before) == before[samples$lastsample])] <- brewercolors["Red"]
    
    ggplot(population,aes(x = before)) + 
      geom_dotplot(aes(fill = ..x..),binwidth = .2) +
      geom_dotplot(fill = samplefill,binwidth = .2) +
      geom_vline(size = .7,
                 aes(xintercept = mean,
                     linetype = paste("Average =",round(mean(before),2)))) +
      scale_fill_gradient(low = "yellow", high = "white",guide = "none") +
      coord_cartesian(xlim = c(0,6)) +
      scale_x_continuous("Colour intensity", breaks = seq(0,6,by = .5)) + 
      scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
      ggtitle("Population: Before") +
      theme_general() + 
      scale_linetype_manual("", values = "dashed") +
      theme(legend.justification = c(1,1),
            legend.position = c(.99,.99))
  })
  
  #After population plot
  output$afterpopplot <- renderPlot({
    samplefill = rep(NA,N)
    samplefill[which(sort(after) == after[samples$lastsample])] = brewercolors["Orange"]
    ggplot(population,aes(x = after)) + 
      geom_dotplot(aes(fill = ..x..),binwidth = .2) + 
      geom_dotplot(fill = samplefill, binwidth = .2) + 
      geom_vline(size = .7,
                 aes(xintercept = mean(after),
                     linetype = paste("Average =",round(mean(after),2)))) +
      coord_cartesian(xlim = c(0,6)) +
      scale_x_continuous("Colour intensity",breaks = seq(0,6,by = .5)) + 
      scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
      scale_fill_gradient(low = "yellow", high = "white",guide = "none") +
      ggtitle("Population: After") +
      theme_general() + 
      scale_linetype_manual("", values = "dashed") +
      theme(legend.justification = c(1,1),
            legend.position = c(.99,.99))
  })
  #Last sample difference plot
  output$lastsampleplot <- renderPlot({
   
    df <- data.frame(difference = samples$singlesamplehist)
    
    
    
    ggplot(df, aes(x = difference)) + 
      geom_dotplot(method = "histodot",
                   fill = "black",
                   binwidth = 0.2,
                   dotsize = 1) + 
      geom_vline(size = .7,
                 aes(xintercept = ifelse(is.na(mean(difference)), 1, mean(difference)),
                     linetype = paste("Average = ",
                                      ifelse(is.na(mean(difference)), "", round(mean(difference), digits = 2))))) +
      coord_cartesian(xlim = c(0,3)) + 
      scale_x_continuous("", breaks = seq(0,6,by = .5)) + 
      scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
      ggtitle("One sample of color differences") +
      theme_general() + 
      scale_linetype_manual("", values = "solid") +
      theme(legend.justification = c(1,1),
            legend.position = c(.98,.98))
  }) 
   #Text showing most recent calculation
  output$calculationtext <- renderText({
    validate(
      need(samples$lastsample != "", "")
    )
    before <- round(before[samples$lastsample],2)
    after <-  round(after[samples$lastsample],2)
    
    paste("<font color=\"#D7191C\">",
          before,
          "</font>",
          "-",
          "<font color=\"#FDAE61\">",
          after,
          "</font>",
          "=",
          round(before - after,digits = 2)
    )
  })
  #Sampling distribution plot
  output$sampdistplot <- renderPlot({
    validate(
      need(samples$meanhist != "", "Please start by drawing 5 cases for the first sample")
    )
    df <- data.frame(difference = samples$meanhist)
    ggplot(df, aes(x = difference)) + 
      geom_histogram(fill = "grey",
                     colour = "black",
                     alpha = .8,
                     binwidth = .1) +
      coord_cartesian(xlim = c(0,2)) + 
      scale_x_continuous(name = "Mean differences", breaks = seq(0,3,by = 0.5)) +
      ggtitle("Sampling distribution of mean differences") +
      theme_general()
  })
})
