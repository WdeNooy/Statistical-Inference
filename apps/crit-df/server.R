library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
   source("../plottheme/styling.R", local = TRUE)
  
  mean <- 5.5 #mean of t dist
  sd <- 1 #sd of variable
  
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
      #t~c~ label right
      geom_text(label = paste("t[c]", " == ", round(tc, digits = 2), sep = ""),
                parse = TRUE,
                aes(x = right * 1.02,
                    y =  0.45),
                hjust = 0,
                size = 5) +
      #t~c~ label left
      geom_text(label = paste("t[c]", " == ", round(-tc, digits = 2), sep = ""),
                parse = TRUE,
                aes(x = left * 0.98,
                    y =  0.45),
                hjust = 1,
                size = 5) +
      #Mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "Mean")) +
      #Left vline
      geom_vline(aes(xintercept = left,
                     linetype = "Threshold")) +
      #Right vline
      geom_vline(aes(xintercept = right,
                     linetype = "Threshold")) +
      #Definition of linetypes
      scale_linetype_manual(name = "",
                            values = c("Mean" = "solid", "Threshold" = "dashed")) +
      #Scaling and double axis definitions
      coord_cartesian(xlim = c(3.5,7.5), ylim = c(0,0.5)) +
      scale_x_continuous(breaks = seq(3.5, 7.5, by = .5),
                         sec.axis = sec_axis(~ (. - mean) / se,
                                             breaks = c(-10, -5, -3, -2, -1, 0 , 1, 2, 3, 5, 10),
                                             name = "t value")) +
      scale_y_continuous(breaks = NULL) + 
      #Axis labels and theme                                       
      xlab("Average media literacy in a sample of children") + 
      ylab("Probability density") + 
      theme_general() + 
      guides(linetype = FALSE)
  })
})
