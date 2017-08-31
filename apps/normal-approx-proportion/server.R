library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
   
#Load styling for plots
source("../plottheme/styling.R", local = TRUE)
  
output$mainplot <- renderPlot({
  
  n <- input$samplesizeslider
  p <- input$propslider
  k <- 1000
  
  samples <- replicate(k,rbinom(n = n, size = 1, prob = p))
  sampmeans <- apply(X = samples,2, mean)
  
  data <- data.frame(sampmeans)

  ggplot(data = data, aes(x = sampmeans)) + 
    geom_histogram(binwidth = 0.02,fill = brewercolors["Yellow"], colour = "black") +
    coord_cartesian(xlim = c(0,1)) + 
    ylab("Count") + 
    xlab("Sample proportion of yellow candies") + 
    stat_function( 
      fun = function(x, mean, sd, n, bw){ 
        dnorm(x = x, mean = mean, sd = sd) * n * bw
      }, 
      args = c(mean = p, sd = sqrt(p*(1-p)/n), n = k, bw = 0.02),
      n = 1000) +
    geom_area(stat = "function", 
      fun = function(x, mean, sd, n, bw){ 
        dnorm(x = x, mean = mean, sd = sd) * n * bw
      }, 
      args = c(mean = p, sd = sqrt(p*(1-p)/n), n = k, bw = 0.02),
      fill = "grey",
      alpha = 0.5) +
    theme_general()
  
  
})
  
})
