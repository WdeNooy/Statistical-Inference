library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
   source("../plottheme/styling.R", local = TRUE)
  
  mean <- 2.8 #mean of t dist
  sd <- 0.5 #sd of variable
  #effect sizes
  d <- c(mean - 0.8*sd, mean - 0.5*sd, mean - 0.2*sd, mean, mean + 0.2*sd, mean + 0.5*sd, mean + 0.8*sd)
  dlabs <- c(
    paste0("Strong\n", format(d[1], nsmall=2)),
    paste0("Moderate\n", format(d[2], nsmall=2)),
    paste0("Weak\n", format(d[3], nsmall=2)),
    paste0("H_0\n", format(d[4], nsmall=2)),
    paste0("Weak\n", format(d[5], nsmall=2)),
    paste0("Moderate\n", format(d[6], nsmall=2)),
    paste0("Strong\n", format(d[7], nsmall=2))
  )
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    df <- input$sampsizeslider - 1 #df
    se <- sd / sqrt(input$sampsizeslider)
    tc <- qt(0.025, df, lower.tail = FALSE)
    
    #Calculating the right and left threshold
    right <- mean + se * tc
    left <- mean - se * tc

    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area under curve
      stat_function(fun = dtshift,
                    xlim = c(1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Right area under curve
      stat_function(fun = dtshift,
                    xlim = c(right,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #2,5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.02 ,
                    y =  dtshift(right, mean, se, df) + 0.01),
                hjust = 0,
                size = 5) +
      #2.5% label left
      geom_text(label = "2.5%",
                aes(x = left * 0.98 ,
                    y =  dtshift(left, mean, se, df) + 0.01),
                hjust = 1,
                size = 5) +
      #Horizontal axis
      geom_hline(aes(yintercept = 0)) +
      #Left vline
      geom_vline(aes(xintercept = left,
                     linetype = "Threshold")) +
      #Right vline
      geom_vline(aes(xintercept = right,
                     linetype = "Threshold")) +
      #Mean vline
      geom_vline(aes(xintercept = d[1]),
                 colour = brewercolors["Red"]) +
      geom_vline(aes(xintercept = d[3]),
                 colour = brewercolors["Red"]) +
      geom_vline(aes(xintercept = d[4]),
                 colour = brewercolors["Red"]) +
      geom_vline(aes(xintercept = d[6]),
                 colour = brewercolors["Red"]) +
      #p value label weak
      geom_text(label = paste0("p", " == ", 
                               format(round(2*pt((d[3]-mean)/se, df = df),
                                            digits = 3), nsmall = 3)),
                parse = TRUE,
                aes(x = d[3] - 0.02,
                    y =  0.45),
                hjust = 1,
                size = 5,
                colour = brewercolors["Red"]) +
      #p value label moderate
      geom_text(label = paste0("p", " == ", 
                               format(round(2*pt((d[2]-mean)/se, df = df),
                                            digits = 3), nsmall = 3)),
                parse = TRUE,
                aes(x = d[6] + 0.02,
                    y =  0.45),
                hjust = 0,
                size = 5,
                colour = brewercolors["Red"]) +
      #p value label strong
      geom_text(label = paste0("p", " == ", 
                               format(round(2*pt((d[1]-mean)/se, df = df),
                                            digits = 3), nsmall = 3)),
                parse = TRUE,
                aes(x = d[1] - 0.02,
                    y =  0.45),
                hjust = 1,
                size = 5,
                colour = brewercolors["Red"]) +
      #Definition of linetypes
      scale_linetype_manual(name = "",
                            values = c("Mean" = "solid", "Threshold" = "dashed")) +
      #Scaling and double axis definitions
      coord_cartesian(xlim = c(2.2, 3.4), ylim = c(0,0.5)) +
      scale_x_continuous(breaks = d, labels = dlabs) +
      scale_y_continuous(breaks = NULL) + 
      #Axis labels and theme                                       
      xlab("Average candy weight") + 
      ylab("Density") +
      ggtitle("Sampling distribution of average candy weight") +
      theme_general() + 
      guides(linetype = FALSE)
  })
})
