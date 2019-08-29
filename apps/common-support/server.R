library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
   
  #Load styling file
  source("../plottheme/styling.R", local = TRUE)
  
  n = 30
  
  data <- reactive({
    input$samplebtn
    
    exposmokers <- seq(from = sample(c(0, 0, 2, 4), size = 1), 
                       to = sample(c(8, 10, 10), size = 1), length.out = n)
    expononsmokers <- seq(from = sample(c(0.5, 0.5, 2.5, 4.5), size = 1), 
                       to = sample(c(8, 10, 10), size = 1), length.out = n)
    expoformersmokers <- seq(from = sample(c(0, 3.2, 5.2), size = 1), 
                       to = sample(c(7.5, 8.5, 10, 10), size = 1), length.out = n)

    smokers <- sample(c(0.3,-0.6,0.7 ), size = 1 )*exposmokers + rnorm(n = n, mean = 3, sd = 1)
    non_smokers <- sample(c(.2,-0.3,0.6 ), size = 1 )*expononsmokers + rnorm(n = n, mean = 3, sd = 1)
    former_smokers <- sample(c(-0.2,-0.6,0.5 ), size = 1 )*expoformersmokers + rnorm(n = n, mean = 3, sd = 1)
    
    df <- 
      data.frame(Exposure = c(exposmokers,expononsmokers,expoformersmokers),
                     Attitude = c(smokers, non_smokers, former_smokers),
                     Group = c(rep("Smoker",n), rep("Non-smoker",n),rep("Former smoker",n))
      )
    
    df
  })
  exposmokers <- seq(from = sample(c(0, 0, 2, 4), size = 1), 
                     to = sample(c(4, 8, 10, 10), size = 1), length.out = n)
  expononsmokers <- seq(from = sample(c(0.5, 0.5, 2.5, 4.5), size = 1), 
                        to = sample(c(4, 8, 10, 10), size = 1), length.out = n)
  expoformersmokers <- seq(from = sample(c(0, 3.2, 5.2), size = 1), 
                           to = sample(c(4.5, 8.5, 10, 10), size = 1), length.out = n)
  
  smokers <- sample(c(0.3,-0.6,0.7 ), size = 1 )*exposmokers + rnorm(n = n, mean = 3, sd = 1)
  non_smokers <- sample(c(.2,-0.3,0.6 ), size = 1 )*expononsmokers + rnorm(n = n, mean = 3, sd = 1)
  former_smokers <- sample(c(-0.2,-0.6,0.5 ), size = 1 )*expoformersmokers + rnorm(n = n, mean = 3, sd = 1)
  
  output$scatterplot <- renderPlot({
    df <- data()
    
    ggplot(df, aes(x = Exposure, y = Attitude, colour = Group)) +
      geom_point() +
      geom_smooth(method="lm", fill=NA, n = 1000, fullrange = TRUE) +
      {if(input$selectinput == "All") scale_colour_manual(values = c("Former smoker" = unname(brewercolors["Orange"]),
                                                                      "Smoker" = unname(brewercolors["Red"]),
                                                                      "Non-smoker" = unname(brewercolors["Blue"])))} + 
      {if(input$selectinput == "Non-smoker") scale_colour_manual(values = c("Former smoker" = "grey",
                                                                      "Smoker" = "grey",
                                                                      "Non-smoker" = unname(brewercolors["Blue"])))} + 
      {if(input$selectinput == "Smoker") scale_colour_manual(values = c("Former smoker" = "grey",
                                                                      "Smoker" = unname(brewercolors["Red"]),
                                                                      "Non-smoker" = "grey"))} + 
      {if(input$selectinput == "Former smoker") scale_colour_manual(values = c("Former smoker" = unname(brewercolors["Orange"]),
                                                                      "Smoker" = "grey",
                                                                      "Non-smoker" = "grey"))} + 
      
      theme_general() +
      xlim(c(0,10))
      
  })
  
  output$histogram <- renderPlot({
    
    df <- data()
    if(input$selectinput != "All"){
     df <- subset(df,Group == input$selectinput) 
    }
    ggplot(df, aes(x = Exposure, fill = Group)) +
     geom_histogram(color = "black", binwidth = .5) +
     {if(input$selectinput == "All") scale_fill_manual(values = c("Former smoker" = unname(brewercolors["Orange"]),
                                                                    "Smoker" = unname(brewercolors["Red"]),
                                                                    "Non-smoker" = unname(brewercolors["Blue"])))} + 
    {if(input$selectinput == "Non-smoker") scale_fill_manual(values = c("Former smoker" = "grey",
                                                                          "Smoker" = "grey",
                                                                          "Non-smoker" = unname(brewercolors["Blue"])))} + 
    {if(input$selectinput == "Smoker") scale_fill_manual(values = c("Former smoker" = "grey",
                                                                      "Smoker" = unname(brewercolors["Red"]),
                                                                      "Non-smoker" = "grey"))} + 
    {if(input$selectinput == "Former smoker") scale_fill_manual(values = c("Former smoker" = unname(brewercolors["Orange"]),
                                                                             "Smoker" = "grey",
                                                                             "Non-smoker" = "grey"))} + 
    xlim(c(0,10)) +
    ylab("Count") +
    theme_general()
    
  })
})
