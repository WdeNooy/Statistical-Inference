library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  source("../plottheme/styling.R",local = TRUE)
  mean = 2.8 #population mean
  sd = runif(n = 1, min = 0.6, max = 1) #Population standard deviation

  output$mainplot <- renderPlot({
    right <- input$mainslider #Left border value
    left <- mean - (input$mainslider - mean) #Right border value
 
    ggplot(data.frame(x = c(0, 6)), aes(x = x)) +
       #Left area under curve
       stat_function(fun = dnorm,xlim = c(-10,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = sd)) + 
      #Center area under curve
      stat_function(fun = dnorm,
                    xlim = c(left,right),
                    geom = "area",
                    fill = "white",
                    args = list(mean = mean, sd = sd)) +
      #Right area under curve
      stat_function(fun = dnorm,
                    xlim = c(right,10),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = sd)) +
      #Normal function line 
      stat_function(fun = dnorm,
                     args = list(mean = mean, sd = sd)) +
      #Left vline
      geom_vline(aes(xintercept = left)) +
      #Right vline
      geom_vline(aes(xintercept = right)) +
      #Mean vline
      geom_vline(aes(xintercept = mean),
                     colour = "grey") +
      # Scale x breaks definition
      scale_x_continuous(breaks = seq(0, 6 ,by = .4), limits = c(0, 6)) +
      #Center text label
      geom_text(label = paste(
                          round(100 - 2*
                            pnorm(left, mean = 2.8, sd = sd, lower.tail = TRUE)*100, 
                          digits = 1),
                        "%", sep = ""),
                aes(x=mean,
                    y = dnorm(mean, mean = mean, sd = sd)/2,
                    vjust = .5
                    )) +
      #Left text label
      geom_text(label = paste(
                          round(
                            pnorm(left, mean = 2.8, sd = sd, lower.tail = TRUE)*100, 
                          digits = 1),
                        "%", sep = ""),
                aes(x=left,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 1
                )) +
      #Right text label
      geom_text(label = paste(
        round(
          pnorm(right, mean = 2.8, sd = sd, lower.tail = FALSE)*100, 
          digits = 1),
        "%", sep = ""),
        aes(x=right,
            y = dnorm(mean, mean = mean, sd = sd)/3,
            hjust = 0
        )) +
      
      #Title and labels for axes
      ggtitle("Sampling distribution") + 
      xlab("Average candy weight per sample") +
      ylab("Density") + 
      #General theme
      theme_general() +
      #Legend positioning
      theme(legend.position = "none")
      
      
    })
})
