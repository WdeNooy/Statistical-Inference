library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Load general plot theme and colors for color brewer
  source("../plottheme/styling.R",local = TRUE)
  
  N <- 100 #population size
  mean <- 3.5 # population before mean
  sd <- 1.2 # population sd red
  
  # container for samples
  samples <- reactiveValues(
    lastsample = as.numeric(),
    hist = as.numeric()
  )
  #Make a before and after population
  before <- rnorm(n = N, mean = mean, sd =sd)
  after <- before - runif(n = N,0,2)
  population <- data.frame(before = before, after = after)
  #Smallsamplebuttonpress
  observeEvent(input$smallsamplebutton,{
    samples$lastsample <<- sample(1:N,1)
    difference <- before[samples$lastsample] - after[samples$lastsample]
    samples$hist <<- c(samples$hist,difference)
  }
  )
  #Largesamplebuttonpress
  observeEvent(input$largesamplebutton,{
    largesample <- sample(1:N,1000,replace = TRUE)
    difference <- before[largesample] - after[largesample]
    samples$lastsample <<- largesample[length(largesample)]
    samples$hist <<- c(samples$hist, difference) 
  }
  )
  #Resetbuttonpress
  observeEvent(input$resetbutton,{
    samples$lastsample <<- as.numeric()
    samples$hist <<- as.numeric()
  }
  )
  
  #Before population plot
  output$beforepopplot <- renderPlot({
    
    samplefill = rep(NA,N)
    samplefill[samples$lastsample] = brewercolors["Red"]
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
      ggtitle("Population of candy colour intensity before") +
      theme_general() + 
      scale_linetype_manual("", values = "dashed") +
      theme(legend.justification = c(1,1),
            legend.position = c(.99,.99))
  })
  
  #After population plot
  output$afterpopplot <- renderPlot({
    samplefill = rep(NA,N)
    samplefill[samples$lastsample] = brewercolors["Orange"]
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
      ggtitle("Population of candy colour intensity after") +
      theme_general() + 
      scale_linetype_manual("", values = "dashed") +
      theme(legend.justification = c(1,1),
            legend.position = c(.99,.99))
  })
  #Last sample difference plot
  output$lastsampleplot <- renderPlot({
    validate(
      need(samples$lastsample != "", "")
    )
    df <- data.frame(difference = before[samples$lastsample] - after[samples$lastsample])
    ggplot(df, aes(x = difference)) + 
      geom_dotplot(method = "histodot",
                   bins = 1,
                   fill = "black",
                   dotsize = 8) + 
      geom_vline(size = .7,
                 aes(xintercept = mean(difference),
                     linetype = paste("Difference = ",
                                      round(mean(difference), digits = 2)))) +
      coord_cartesian(xlim = c(0,6)) + 
      scale_x_continuous("Weight", breaks = seq(0,6,by = .5)) + 
      scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
      ggtitle("Last sample of colordifference") +
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
      need(samples$hist != "", "Please start by drawing a sample")
    )
    df <- data.frame(difference = samples$hist)
    ggplot(df, aes(x = difference)) + 
      geom_histogram(fill = "grey",
                     colour = "black",
                     alpha = .8,
                     binwidth = .1) +
      coord_cartesian(xlim = c(0,3)) + 
      scale_x_continuous(name = "Mean differences", breaks = seq(0,3,by = 0.5)) +
      ggtitle("Sampling distribution of mean differences") +
      theme_general()
  })
})
