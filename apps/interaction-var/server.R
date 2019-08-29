library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  #Load styling for plots
  source("../plottheme/styling.R", local = TRUE)
  # #Set constant.
  # contact_mean = 4.5
  
  #Group size (2 groups, each of this size)
  n = 45
  #Generate group identifier.
  Group <- c(rep("Smoker = 1",n), rep("Non-smoker = 0",n))
  #Generate dummies.
  smoker <- ifelse(Group == "Smoker = 1", 1, 0)
  
  data <- reactive({
    input$samplebtn
    
    #Generate random exposure scores between 0 and 10 for all observations.
    Exposure <- runif(2*n, min = 0.1, max = 9.8)
    
    #Generate attitude outcome with or without Group interactions.
    #With different means for different groups.
    Attitude <- -0.7 * Exposure + #general negative effect
      2.8 * smoker + #average group differences
      ifelse(runif(1, 0, 1) < .4, runif(1, -0.8, -0.4), runif(1, 0.4, 0.8)) * smoker * Exposure + #interaction smokers
      rnorm(n = 2*n, mean = 0, sd = 1) #random error
    #Rescale Attitude to [-5, 5]
    Attitude <- ((Attitude - min(Attitude)) / ((max(Attitude) - min(Attitude)) / 10)) - 5
    
    #Collect in dataframe
    df <- data.frame(Exposure, Attitude, Group)
    
    df
  })
  
  output$scatterplot <- renderPlot({
    df <- data()
    
    #results of linear model
    results <- lm(Attitude ~ Exposure*Group, data = df)$coefficients
    
    ggplot(df) +
      geom_point(aes(x = Exposure, y = Attitude, colour = Group)) +
      #line for non-smokers
      geom_abline(
        intercept = results[[1]],
        slope = results[[2]],
        colour = unname(brewercolors["Blue"])
      ) +
      #target line for smokers
      geom_abline(
        colour = "grey",
        intercept = results[[1]] + results[[3]],
        slope = results[[2]] + results[[4]]
      ) +
      #line for smokers
      geom_abline(
        colour = unname(brewercolors["Red"]),
        intercept = results[[1]] + results[[3]] + (results[[4]] - input$coefficientslider) * mean(df$Exposure[df$Group == "Smoker = 1"]),
        slope = results[[2]] + input$coefficientslider
      ) +
      # geom_smooth(method="lm", fill=NA, n = 1000, fullrange = FALSE) +
      scale_colour_manual(values = c("Smoker = 1" = unname(brewercolors["Red"]),
                                     "Non-smoker = 0" = unname(brewercolors["Blue"]))) +
      theme_general() +
      theme(legend.position = "bottom") +
      xlim(c(0,10)) +
      ylim(c(-5, 5))
  })
  
  ##FORMULA OUTPUT##
  output$formulamult <- renderUI({
    df <- data()
    #results of linear model
    results <- lm(Attitude ~ Exposure*Group, data = df)$coefficients
    withMathJax(
      helpText(
        paste0("$$\\small{attitude = ",
              round(as.numeric(results[[1]]),2), 
              " + ",
              round(as.numeric(results[[2]]),2),
              " * exposure + ",
              round(as.numeric(results[[3]]),2),
              " * smoker + \\color{blue}{",
              round(as.numeric(input$coefficientslider),2),
              "} * exposure * smoker}$$")
      )
    )
  })
  # output$formulasimple <- renderUI({
  #   withMathJax(
  #     helpText(
  #       paste("$$\\color{blue}{attitude = ",
  #             0.65 + 0.5 * as.numeric(input$smokeselector) + 0.15 * input$coefficientslider,
  #             "- 0.6 * exposure}$$")
  #     )
  #   )
  # })

})