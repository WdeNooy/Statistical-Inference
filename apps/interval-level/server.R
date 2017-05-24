library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  source("../plottheme/styling.R",local = TRUE)
  n = 30 # sample size
  mean = 2.8 #population mean
  sd = runif(n = 1, min = 0.2, max = 1) #Population standard deviation

  output$mainplot <- renderPlot({
    tailarea <- (1 - input$mainslider / 100) / 2 #calculates value for qnorm
    error <- qnorm(1 - tailarea) * sd # Distance from mean
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
 
    validate(
      need(input$mainslider != 100,
            "Selecting 100% leads to an infinitely wide confidence interval")
    )
    
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
      geom_vline(aes(xintercept = left,
                 linetype = "left margin")) +
      #Right vline
      geom_vline(aes(xintercept = right,
                 linetype = "right margin")) +
      #Mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "mean")) + 
      #Defining types of lines
      scale_linetype_manual(name = "",
                            values = c("left margin" = "dashed",
                                       "right margin" = "dashed",
                                       "mean" = "solid")) + 
      # Scale x breaks definition
      scale_x_continuous(breaks = seq(0, 6 ,by = .4), limits = c(0, 6)) +
      #Center text label
      geom_text(label = paste(input$mainslider, "%", sep = ""),
                aes(x=mean,
                    y = dnorm(mean, mean = mean, sd = sd)/2,
                    vjust = .5
                    )) +
      #Left text label
      geom_text(label = paste((100 - input$mainslider)/2, "%", sep = ""),
                aes(x=left,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 1
                )) +
      #Right text label
      geom_text(label = paste((100 - input$mainslider)/2, "%", sep = ""),
                aes(x=right,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 0
                )) +
      
      #N text label
      geom_text(label = paste("n = ", n, sep = ""),
                aes(x=6,
                    y = dnorm(mean, mean = mean, sd = sd)* .8,
                    hjust = 1
                )) +
      #Left arrow
      geom_segment(x = mean,
                    xend = left,
                   y = dnorm(mean, mean = mean, sd = sd) * .1,
                   yend = dnorm(mean, mean = mean, sd = sd) * .1,
                   arrow = arrow(length = unit(.3,"cm"))) + 
      #Right arrow
      geom_segment(x = mean,
                   xend = right,
                   y = dnorm(mean, mean = mean, sd = sd) * .1,
                   yend = dnorm(mean, mean = mean, sd = sd) * .1,
                   arrow = arrow(length = unit(.3,"cm"))) +
      #Title and labels for axes
      ggtitle("Sampling distribution") + 
      xlab("Average candy weight per sample") +
      ylab("Density") + 
      #General theme
      theme_general() +
      #Legend positioning
      theme(legend.position = "bottom",
            legend.direction = "horizontal")
      
      
    })
})
