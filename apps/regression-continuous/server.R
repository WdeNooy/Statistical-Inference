library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  #Load styling for plots
  source("../plottheme/styling.R", local = TRUE)

  #CREATE PREDICTOR
  n <- 100 #number of data points
  set.seed(4932)
  exposure <- rnorm(n, mean = 4, sd = 2) * 0.8
  # Create outcome.
  set.seed(391)
  attitude <-
    1.6 - 0.6 * exposure + rnorm(n, mean = 0, sd = 0.9)

  # attfun <- function(exposure, contact, smoker) {
  #   0.65 - 0.6 * exposure + 0.5 * smoker + 0.15 * contact
  # }
  
  output$mainplot <- renderPlot({
  
    #Create data frames for plotting
    df <- data.frame(x = c(0, 6)) #Limits for line
    scatter <- data.frame(attitude = attitude, exposure = exposure)
    #Plot
    ggplot(scatter,aes(x = exposure, y = attitude)) +
      #dots
      geom_point(data = scatter,
                 shape = 21,
                 size = 3,
                 aes(x = exposure,
                     y = attitude)) +
      #regression line
      #horizontal line for predicted value
      geom_hline(yintercept = 1.6 - 0.6 * input$expovalueslider, color = "grey") +
      geom_abline(slope = -0.6, intercept = 1.6, color = brewercolors[5], size = 1) + 
      #vertical line at selected exposure
      geom_vline(xintercept = input$expovalueslider, color = "grey") +
      #contribution of b to the prediction
      geom_label(aes(x = 0, y = 1.6 - 0.3 * input$expovalueslider, 
                     label = ifelse(input$expovalueslider == 0, "", paste0(input$expovalueslider, "*b")), 
                     hjust = 0), label.size = 0,
                 color = brewercolors[2], size = 6) +
      #vertical segment
      geom_segment(aes(x = 0, xend = 0, y = 1.6, yend = 1.6 - 0.6 * input$expovalueslider),
                   size = 2,
                   color = brewercolors[2]) +
      #predicted value
      geom_point(aes(x = input$expovalueslider, y = 1.6 - 0.6 * input$expovalueslider),
                color = brewercolors[2], size = 6) +
      scale_x_continuous(name = "Exposure", limits = c(0, 10), breaks = 0:10) +
      scale_y_continuous(name = "Attitude", limits = c(-5, 5), breaks = c (-5, 0, 1.6, 5, 1.6 - 0.6 * input$expovalueslider)) +
      theme_general()
    
  })
  
  ##FORMULA OUTPUT##
  output$formulaui <- renderUI({
    withMathJax(
      helpText(
        paste("$$\\color{orange}{attitude}\\color{black}{ = constant + b * }\\color{steelblue}{exposure}$$ \n $$\\color{orange}{",1.6 - 0.6 * input$expovalueslider,"}\\color{black}{ = 1.6 + -0.6 * }\\color{steelblue}{",input$expovalueslider,"}$$")
      )
    )
  })
})