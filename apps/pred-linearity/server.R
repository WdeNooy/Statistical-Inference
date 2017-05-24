library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  #Load styling file
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 20 #Number of observations
  x <- seq(from = 0, to = 10, length.out = n)
  
  #Select data generating function depending on dropdown menu
  data <- reactive({
    switch(input$typeselector,
           "Linear" = -0.6*x + rnorm(n = n, mean = 3, sd = 1),
           "Curved" = .08*x^2 - rnorm(n = n, mean = 4, sd = 0.4),
           "U shaped" = .3*(x-5)^2 - rnorm(n = n, mean = 3, sd = 1))
  })
  ##SCATTERPLOT## 
  output$scatterplot <- renderPlot({
    #Fit model
    df = data.frame(exposure = x, attitude = data())
    fit <- lm(attitude ~ exposure, data = df)
    
    #Extract predicted and residuals
    df$predicted <- predict(fit)
    df$resid <- residuals(fit)
    
    #PLOT
    ggplot(df, aes(x = exposure, y = attitude)) + 
      geom_smooth(method='lm',
                  formula=y~x,
                  se = FALSE) +
      geom_segment(aes(xend = exposure,
                       yend = predicted),
                   color = brewercolors["Red"]) +
      geom_point(shape = 21, size = 3) +
      xlab("Exposure") +
      ylab("Attitude") +
      theme_general()
    
  })
  ##PRED VS RESID PLOT## 
  output$residplot <- renderPlot({
    #Fit model
    df <- data.frame(exposure = x, attitude = data())
    fit <- lm(attitude ~ exposure, data = df)
    #Extract pred and resid
    df$predicted <- predict(fit)
    df$resid <- residuals(fit)
    
    #Make variable showing points in rectangle
    df <- brushedPoints(df, input$scatterbrush, allRows = TRUE)
    
    #PLOT
    ggplot(df, aes(x = predicted, y = resid, colour = selected_)) +
      scale_colour_manual(values=c("black", unname(brewercolors["Red"]))) +
      geom_segment(data =subset(df, selected_ == TRUE),
                     aes(xend = predicted),
                         yend = 0,
                   color = unname(brewercolors["Red"])) +
      geom_point(shape = 21,
                 size = 3) + 
      geom_hline(yintercept = 0) +
      xlab("Predicted attitude") +
      ylab("Residual") +
      theme_general() +
      theme(legend.position = "none")

  })
})
