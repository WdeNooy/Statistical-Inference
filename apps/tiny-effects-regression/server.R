library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Source styling file for plots
  source("../plottheme/styling.R", local = TRUE)

  mean <- 0 #Hypothesized population mean
  sdpop <- 0.55 #Population SD 
  
  ##RENDER MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #Hardcoded labels for plot
    strengthlab <- c("Strong\n-0.24",
                     "Moderate\n-0.15",
                     "Weak\n-0.06",
                     "H0\n0",
                     "Weak\n0.06",
                     "Moderate\n0.15",
                     "Strong\n0.24")
    #Hardcoded tickmarks for plot
    ticks <- c(-0.24, -0.15, -0.06, 0, 0.06, 0.15, 0.24) 
    
    SE <- sdpop/sqrt(input$ssizeslider) #SE
    
    error <- qnorm(1 - .025) * SE # Distance from mean
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
    
    sign <- ifelse(right <= input$savslider, 
                   "Statistically\n significant\n test result", 
                   "Statistically\n non-significant\n test result")
    
     #PLOT
     ggplot(data.frame(x = c(-1,1)), aes(x = x)) +
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
      #Sampling distribution mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "Hypothesized population b")) +
      #Sample mean vline
      geom_vline(aes(xintercept = input$savslider,
                      linetype = "Sample b"),
                 colour = brewercolors["Red"]) +
      #Test result label
      geom_text(label = sign,
                aes(x = 0.15, 
                    y = dnorm(mean, mean, SE)),
                vjust = 1,
                color = brewercolors["Blue"]
                ) +
      #Definition of sample mean type and legend name
      scale_linetype_manual(name = "",
                            values = c("Hypothesized population b" = "dashed", 
                                       "Sample b" = "solid")) + 
      #X axis breaks definition
      scale_x_continuous(breaks = ticks, labels = strengthlab) + 
      #Defining x axis zoom
      coord_cartesian(xlim = c(-0.3, 0.3)) +
      #Title and labels for axes
      ggtitle("Sampling distribution") + 
      xlab("Unstandardized sample regression coefficient") + 
      ylab("Density") +
      #Theme specification
      theme_general() + 
      theme(axis.text.x = element_text(size = 9),
            legend.position = "top",
            legend.direction = "horizontal")
  })
})
