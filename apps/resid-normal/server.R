library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  #Load styling file
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 70 #Number of observations
  x <- seq(from = 0, to = 10, length.out = n)
  
  
 data <- reactive({
   input$samplebutton
   -0.6*x + rnorm(n = n, mean = 3, sd = 1)
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
  
  output$residplot <- renderPlot({
    
    df <- data.frame(exposure = x, attitude = data())
    fit <- lm(attitude ~ exposure, data = df)
    df$resid <- residuals(fit)
    
    df <- brushedPoints(df, input$scatterbrush, allRows = TRUE)
    
    ggplot(df, aes(x = resid, fill = selected_)) + 
      geom_histogram(colour = "black", binwidth = 0.3) + 
      stat_function( 
        fun = function(x, mean, sd, n, bw){ 
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        }, 
        args = c(mean = 0, sd = 1, n = n, bw = 0.3)) +
      scale_fill_manual(values = c("TRUE" = unname(brewercolors["Blue"]),
                                   "FALSE" = unname(brewercolors["Red"])),
                        limits = c("TRUE", "FALSE"),
                        breaks = c("TRUE","FALSE")) + 
      xlab("Residuals") + 
      ylab("Count") +
      theme_general() +
      theme(legend.position = "none")
  })
})
