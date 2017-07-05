library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  source("../plottheme/styling.R",local = TRUE)
  cilevel = .95 # confidence level
  n = 25 #sample size (just for reporting)
  mean = 2.8 #population mean

  output$mainplot <- renderPlot({
    se <- input$mainslider # sets standard error
    error <- qnorm((1 - cilevel)/2) * se # Distance from mean
    left <- mean + error #Left confidence interval border
    right <- mean - error #Right confidence interval border
 
    ggplot(data.frame(x = c(0, 6)), aes(x = x)) +
       #Left area under curve
       stat_function(fun = dnorm,xlim = c(-10,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = se)) + 
      #Center area under curve
      stat_function(fun = dnorm,
                    xlim = c(left,right),
                    geom = "area",
                    fill = "white",
                    args = list(mean = mean, sd = se)) +
      #Right area under curve
      stat_function(fun = dnorm,
                    xlim = c(right,10),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = se)) +
      #Normal function line 
      stat_function(fun = dnorm,
                     args = list(mean = mean, sd = se)) +
      #-1.96 vline
      geom_vline(aes(xintercept = left,
                 linetype = "helpline")) +
      #-1.00 vline
      geom_vline(aes(xintercept = mean - se,
                     linetype = "helpline")) +
      #1.00 vline
      geom_vline(aes(xintercept = mean + se,
                     linetype = "helpline")) +
      #1.96 vline
      geom_vline(aes(xintercept = right,
                 linetype = "helpline")) +
      #Mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "mean")) + 
      #Defining types of lines
      scale_linetype_manual(name = "",
                            values = c("mean" = "solid", "helpline" = "dashed")) + 
      # Scale x breaks definition
      scale_x_continuous(breaks = seq(0, 6, by = .4), limits = c(0, 6),
                         sec.axis = sec_axis(~ (. - mean) / se,
                                             breaks = c(-1.96, -1, 0 , 1, 1.96),
                                             name = "Average candy weight per sample in standard errors (z scores)")
                         ) +
      #Center text label
      geom_text(label = paste(cilevel * 100, "%", sep = ""),
                aes(x=mean,
                    y = dnorm(mean, mean = mean, sd = se)/2,
                    vjust = .5
                    )) +
      #Left text label
      geom_text(label = paste((100 - 100*cilevel)/2, "%", sep = ""),
                aes(x=left,
                    y = dnorm(mean, mean = mean, sd = se)/3,
                    hjust = 1
                )) +
      #Right text label
      geom_text(label = paste((100 - 100*cilevel)/2, "%", sep = ""),
                aes(x=right,
                    y = dnorm(mean, mean = mean, sd = se)/3,
                    hjust = 0
                )) +
      
      #N text label
      geom_text(label = paste("n = ", n, sep = ""),
                aes(x=6,
                    y = dnorm(mean, mean = mean, sd = se)* .8,
                    hjust = 1
                )) +
      #SE text label
      geom_text(label = paste("se = ", round(se, digits = 2), sep = ""),
                aes(x=6,
                    y = dnorm(mean, mean = mean, sd = se)* .6,
                    hjust = 1
                )) +
      #Left arrow
      geom_segment(x = mean,
                    xend = left,
                   y = dnorm(mean, mean = mean, sd = se) * .1,
                   yend = dnorm(mean, mean = mean, sd = se) * .1,
                   arrow = arrow(length = unit(.3,"cm"))) + 
      #Right arrow
      geom_segment(x = mean,
                   xend = right,
                   y = dnorm(mean, mean = mean, sd = se) * .1,
                   yend = dnorm(mean, mean = mean, sd = se) * .1,
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
