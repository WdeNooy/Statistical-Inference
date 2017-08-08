library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
   
  #Load styling file
  source("../plottheme/styling.R", local = TRUE)
  
  n = 30
  
  data <- reactive({
    input$samplebtn
    
    exposurelow <- seq(from = 1, to = 3, length.out = n)
    exposurehigh <- seq(from = 5, to = 10, length.out = n)
    exposurecomp <- seq(from = 0, to = 10, length.out = n)
    
    smokers <- sample(c(0.3,-0.6,0.7 ), size = 1 )*exposurehigh + rnorm(n = n, mean = 3, sd = 1)
    non_smokers <- sample(c(.2,-0.3,0.6 ), size = 1 )*exposurecomp + rnorm(n = n, mean = 3, sd = 1)
    former_smokers <- sample(c(-0.2,-0.6,0.5 ), size = 1 )*exposurecomp + rnorm(n = n, mean = 3, sd = 1)
    
    df <- 
      data.frame(exposure = c(exposurehigh,exposurecomp,exposurecomp),
                     attitude = c(smokers, non_smokers, former_smokers),
                     group = c(rep("Smoker",n), rep("Non-smoker",n),rep("Former smoker",n))
      )
    
    df
  })
  exposurelow <- seq(from = 1, to = 3, length.out = n)
  exposurehigh <- seq(from = 6, to = 10, length.out = n)
  exposurecomp <- seq(from = 0, to = 10, length.out = n)
  
  smokers <- sample(c(0,-0.6,0.7 ), size = 1 )*exposurehigh + rnorm(n = n, mean = 3, sd = 1)
  non_smokers <- sample(c(0,-0.6,0.7 ), size = 1 )*exposurelow + rnorm(n = n, mean = 3, sd = 1)
  former_smokers <- sample(c(0,-0.6,0.7 ), size = 1 )*exposurehigh + rnorm(n = n, mean = 3, sd = 1)
  
  output$scatterplot <- renderPlot({
    df <- data()
    
    ggplot(df, aes(x = exposure, y = attitude, colour = group)) +
      geom_point() +
      geom_smooth(method="lm", fill=NA, n = 1000) +
      {if(input$selectinput == "All") scale_colour_manual(values = c("Former smoker" = unname(brewercolors["Blue"]),
                                                                      "Smoker" = unname(brewercolors["Green"]),
                                                                      "Non-smoker" = unname(brewercolors["Red"])))} + 
      {if(input$selectinput == "Non-smoker") scale_colour_manual(values = c("Former smoker" = "grey",
                                                                      "Smoker" = "grey",
                                                                      "Non-smoker" = unname(brewercolors["Red"])))} + 
      {if(input$selectinput == "Smoker") scale_colour_manual(values = c("Former smoker" = "grey",
                                                                      "Smoker" = unname(brewercolors["Green"]),
                                                                      "Non-smoker" = "grey"))} + 
      {if(input$selectinput == "Former smoker") scale_colour_manual(values = c("Former smoker" = unname(brewercolors["Blue"]),
                                                                      "Smoker" = "grey",
                                                                      "Non-smoker" = "grey"))} + 
      
      theme_general()
      
  })
  
  output$histogram <- renderPlot({
    
    df <- data()
    if(input$selectinput != "All"){
     df <- subset(df,group == input$selectinput) 
    }
    ggplot(df, aes(x = exposure, fill = group)) +
     geom_histogram(colour = "black", binwidth = .5) +
     {if(input$selectinput == "All") scale_fill_manual(values = c("Former smoker" = unname(brewercolors["Blue"]),
                                                                    "Smoker" = unname(brewercolors["Green"]),
                                                                    "Non-smoker" = unname(brewercolors["Red"])))} + 
    {if(input$selectinput == "Non-smoker") scale_fill_manual(values = c("Former smoker" = "grey",
                                                                          "Smoker" = "grey",
                                                                          "Non-smoker" = unname(brewercolors["Red"])))} + 
    {if(input$selectinput == "Smoker") scale_fill_manual(values = c("Former smoker" = "grey",
                                                                      "Smoker" = unname(brewercolors["Green"]),
                                                                      "Non-smoker" = "grey"))} + 
    {if(input$selectinput == "Former smoker") scale_fill_manual(values = c("Former smoker" = unname(brewercolors["Blue"]),
                                                                             "Smoker" = "grey",
                                                                             "Non-smoker" = "grey"))} + 
    xlim(c(0,10)) + 
    theme_general()
    
  })
})
