library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Source styling file for plots
  source("../plottheme/styling.R", local = TRUE)

  mean <- 2.8 #Population mean
  sdpop <- 0.6 #Population SD 
  
  #Container for reactive values
  sample <- reactiveValues(lastsample = numeric(),
                           SE = numeric(),
                           power = numeric())
  
  #Actions to execute if sample button is pressed.
  #ignoreNULL = FALSE makes that the actions execute on loading.
  observeEvent(input$sampbutton, ignoreNULL = FALSE,{
    #Sample 5 times samplesize slider from normal distribution
    temp <-
      replicate(rnorm(input$ssizeslider, mean = input$savslider, sd = sdpop),
                n = 5)
    #Store the means of the samples in lastsample
    sample$lastsample <<- data.frame(means = apply(temp,2,mean))
    #Calculate the standard error and store 
    sample$SE <<- sdpop/sqrt(input$ssizeslider)
    #calculate power and store
    sample$power <<- ifelse(
      input$savslider == mean, NA, round(
        power.t.test(n = input$ssizeslider,
          delta = (input$savslider - mean),
          sd = sdpop,
          sig.level = 0.05,
          type = "one.sample",
          alternative = "two.sided")$power,
        2)
    )
      
  })
  

  
  ##RENDER MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #Hardcoded labels for plot
    strengthlab <- c("Strong\n2.32",
                     "Moderate\n2.5",
                     "Weak\n2.68",
                     "H0\n2.8",
                     "Weak\n2.92",
                     "Moderate\n3.1",
                     "Strong\n3.28")
    #Hardcoded tickmarks for plot
    ticks <- c(2.32,2.5,2.68,2.8,2.92,3.1,3.28) 
    
    df <- sample$lastsample #Lastsample
    SE <- sample$SE #SE
    
    error <- qnorm(1 - .025) * SE # Distance from mean
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
    
     #PLOT
     ggplot(data.frame(x = c(0,6)), aes(x = x)) +
      #Left area under curve
      stat_function(fun = dnorm,xlim = c(-10,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = SE),
                    n = 1000) + 
      #Right area under curve
      stat_function(fun = dnorm,
                    xlim = c(right,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = SE),
                    n = 1000) +
      #Normal function line 
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = SE),
                    n = 1000) +
      #2,5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.01 ,
                    y =  dnorm(right, mean, SE)),
                hjust = 0,
                size = 5) +
      #2.5%label left
      geom_text(label = "2.5%",
                aes(x = left * 0.99 ,
                    y =  dnorm(left, mean, SE)),
                hjust = 1,
                size = 5) +
      #Power text
      geom_text(label = paste("Power:", sample$power),
                aes(x = right,
                     y = .7 * dnorm(mean, mean,SE)),
                hjust = 0,
                size = 4) +
      #Sampel mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "Mean")) +
      #Definition of sample mean type and legend name
      scale_linetype_manual(name = "",
                            values = c("Mean" = "dashed")) + 
      #Dots for mean of 5 samples
       geom_point(data = df,
                  aes(x = means, y = .1),
                  shape = 21,
                  size = 4,
                  fill = brewercolors) +
      #X axis breaks definition
      scale_x_continuous(breaks = ticks, labels = strengthlab) + 
      #Defining x axis zoom
      coord_cartesian(xlim = c(2.15, 3.45)) +
      #Title and labels for axes
      ggtitle("Sampling distribution") + 
      xlab("Average candy weight") + 
      ylab("Denisty") +
      #Theme specification
      theme_general() + 
      theme(axis.text.x = element_text(size = 9),
            legend.position = "top",
            legend.direction = "horizontal")
  })
})
