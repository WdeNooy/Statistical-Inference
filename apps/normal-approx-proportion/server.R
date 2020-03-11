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
    geom_histogram(aes(y = ..density..),
                   binwidth = 1/n,
                   fill = brewercolors["Yellow"], 
                   colour = "black") +
    stat_function(
      fun = function(x, mean, sd){
        dnorm(x = x, mean = mean, sd = sd)
      },
      args = c(mean = p, sd = sqrt(p*(1-p)/n)),
      n = 500) +
    geom_area(stat = "function",
      fun = function(x, mean, sd){
        dnorm(x = x, mean = mean, sd = sd)
      },
      args = c(mean = p, sd = sqrt(p*(1-p)/n)),
      fill = "grey",
      alpha = 0.5) +
    scale_x_continuous(name = "Proportion of yellow candies in the sample",
                       breaks = c(0, .25, .5, .75, 1),
                       limits = c(-.2, 1.2)) +
    scale_y_continuous(name = "Count", breaks = NULL) +
    theme_general()
  
  
})
  
})
