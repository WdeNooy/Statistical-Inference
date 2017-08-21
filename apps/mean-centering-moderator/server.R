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
  attfun <- function(exposure, contact, betaexposure = -0.26,betacontact = 0.15,betamoderation = 0.04){
    betaexposure*exposure + betacontact*contact + betamoderation*exposure*contact + 0.4
  }
  

  
  ##OUTPUT OF HEADER 
  output$headtext <-renderText(paste("Attitude predicted from Exposure for Contact set to",
                                     input$modvalueslider))
  slidertemp <- numeric()
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
  
    slidertemp <<- c(input$modvalueslider,slidertemp[1])
    #Create data frames for plotting
    df <- data.frame(x = c(0, 6)) #Limits for line
    scatter <- data.frame(attitude = attitude, exposure = exposure)
    scatter$plane <- dnorm(contact, input$modvalueslider + input$modcenterslider/2, sd = .5)
    
    #Plot
    ggplot(df, aes(x = x)) +
      geom_point(data = scatter,
                 shape = 21,
                 size = 3,
                 aes(x = exposure,
                     y = attitude,
                     fill = plane)) +
      scale_fill_gradient(name = "Relevance for current line",low = "white", high = unname(brewercolors["Blue"])) + 
      guides(fill = guide_colorbar(title.position = "top",
                                   barwidth = 9,
                                   order = 1,
                                   label = FALSE,
                                   ticks = FALSE
      )) +
      stat_function(
        fun = attfun,
        args = list(contact = input$modvalueslider + input$modcenterslider/2),
        n = 500,
        alpha = 1,
        size = .9,
        aes(color = "Current line")) +
      scale_color_manual(name = "",
                         values = c("Current line" = unname(brewercolors["Red"])
                         )) +
      coord_cartesian(xlim = c(0, 10), ylim = c(-5, 5)) +
      ylab("Attitude") +
      xlab("Exposure") +
      theme_general() + 
      theme(legend.position = "bottom")
    
  })
  ##EQ OUTPUT##
  output$eqbef <- renderUI({
    withMathJax(
      helpText(
        paste("$$\\color{black}{attitude = 0.4 + (-0.26 + 0.04 * }\\color{blue}{",
              input$modvalueslider,
              "}\\color{black}{)*exposure + 0.15*}\\color{blue}{",
              input$modvalueslider,"}$$")
      )
    )
  })
  
  ##EQ OUTPUT##
  output$eqafter <- renderUI({
    withMathJax(
      helpText(
        paste("$$\\color{black}{attitude = 0.4 + (-0.26 + 0.04 * }\\color{blue}{",
              input$modvalueslider + input$modcenterslider/2,
              "}\\color{black}{)*exposure + 0.15*}\\color{blue}{",
              input$modvalueslider + input$modcenterslider/2,"}$$")
      )
    )
  })
})
