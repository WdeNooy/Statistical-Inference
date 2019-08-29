library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
   
  #Load styling file
  source("../plottheme/styling.R", local = TRUE)
  
  #Group size (3 groups, each of this size)
  n = 30
  
  data <- reactive({
    input$samplebtn
    
    #Generate group identifier.
    Group = c(rep("Smoker",n), rep("Non-smoker",n),rep("Former smoker",n))
    #Generate dummies.
    smoker <- ifelse(Group == "Smoker", 1, 0)
    nonsmoker <- ifelse(Group == "Non-smoker", 1, 0)
    
    #Generate random exposure scores between 0 and 10 for all observations.
    Exposure <- runif(3*n, min = 0.1, max = 9.8)
    
    #Generate attitude outcome with or without Group interactions.
    #With different means for different groups.
    Attitude <- -0.7 * Exposure + #general negative effect
      2.8 * smoker + 1.9 * nonsmoker + #average group differences
      ifelse(runif(1, 0, 1) < .6, runif(1, -0.02, 0.02), runif(1, 0.4, 0.8)) * smoker * Exposure + #interaction smokers
      ifelse(runif(1, 0, 1) < .8, runif(1, -0.02, 0.02), runif(1, 0.1, 0.4)) * nonsmoker * Exposure + #interaction non-smokers
      rnorm(n = 3*n, mean = 0, sd = 1) #random error
    #Rescale Attitude to [-5, 5]
    Attitude <- ((Attitude - min(Attitude)) / ((max(Attitude) - min(Attitude)) / 10)) - 5
    
    #Collect in dataframe
    df <- data.frame(Exposure, Attitude, Group)

    df
  })
  
  output$scatterplot <- renderPlot({
    df <- data()
    
    ggplot(df, aes(x = Exposure, y = Attitude, colour = Group)) +
      geom_point() +
      geom_smooth(method="lm", fill=NA, n = 1000, fullrange = FALSE) +
      scale_colour_manual(values = c("Former smoker" = unname(brewercolors["Orange"]),
                                     "Smoker" = unname(brewercolors["Red"]),
                                     "Non-smoker" = unname(brewercolors["Blue"]))) +
      theme_general() +
      theme(legend.position = "bottom") +
      xlim(c(0,10)) +
      ylim(c(-5, 5))
  })
  
})
