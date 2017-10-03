library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  
  #Load styling for plots
  source("../plottheme/styling.R", local = TRUE)
  
  #CREATE PREDICTOR
  n <- 600 #number of data points
  set.seed(4932)
  exposure <- runif(n) * 10
  # Create moderator.
  set.seed(4321)
  contact <-  0.12 * (10 - exposure) + rnorm(n, mean = 4.5, sd = 2)
  # Create outcome.
  set.seed(391)
  attitude <-  -0.26 * exposure + 0.15 * contact + 0.04 * exposure * contact + rnorm(n, mean = 0.4, sd = 0.9) 

  #Function for calculating attitude line
  attfun <- function(exposure, contact, constant = 0.4, betaexposure = -0.26,betacontact = 0.15,betamoderation = 0.04){
    betaexposure*exposure + betacontact*contact + betamoderation*contact*exposure + constant
  }
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
  
    #Create data frames for plotting
    df <- data.frame(x = c(0, 6)) #Limits for line
    scatter <- data.frame(attitude = attitude, exposure = exposure)
    scatter$plane <- dnorm(contact, input$modcenterslider/2, sd = .5)
    
    #Plot
    ggplot(df, aes(x = x)) +
      geom_point(data = scatter,
                 shape = 21,
                 size = 3,
                 aes(x = exposure,
                     y = attitude,
                     fill = plane),
                 show.legend = FALSE) +
      scale_fill_gradient(name = "",low = "white", high = unname(brewercolors["Blue"])) + 
      stat_function(
        fun = attfun,
        args = list(contact = input$modvalueslider),
        n = 500,
        alpha = 1,
        size = 1.8,
        aes(color = "Contact = 0")) +
      stat_function(
        fun = attfun,
        args = list(contact = 0, 
                    constant = 0.4 + 0.15 * input$modcenterslider/2,
                    betaexposure = -0.26 + 0.04 * input$modcenterslider/2),
        n = 500,
        alpha = 1,
        size = .9,
        aes(color = "Contact centered = 0")) +
      scale_color_manual(name = "",
                         values = c("Contact centered = 0" = unname(brewercolors["Red"]),
                                    "Contact = 0" = unname(brewercolors["Blue"])),
                         labels = c(paste("Contact =", input$modvalueslider), "Contact centered = 0")) +
      geom_text(aes(x = 5, y = -4.0, color = "Contact centered = 0"), size = 2.8,
                label = paste("Attitude = ",  0.4 + 0.15 * input$modcenterslider/2,
                              " + ", -0.26 + 0.04 * input$modcenterslider/2,
                              " * Exposure + 0.15 * Contact + 0.04 * Contact * Exposure"),
                show.legend = FALSE) +
      geom_text(aes(x = 5, y = -4.8, color = "Contact = 0"), size = 2.8,
                label = paste0("Attitude = 0.4 + -0.26 * Exposure + 0.15 * Contact (",
                              input$modvalueslider,
                              ") + 0.04 * Contact (", input$modvalueslider,
                              ") * Exposure"),
                show.legend = FALSE) +
      coord_cartesian(xlim = c(0, 10), ylim = c(-5, 5)) +
      ylab("Attitude") +
      xlab("Exposure") +
      theme_general() + 
      theme(legend.position = "bottom")
    
  })
})
