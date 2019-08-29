library(shiny)
library(dplyr)
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
  contact <- 0.12 * (10 - exposure) + rnorm(n, mean = 4.5, sd = 2)
  
  # Create outcome.
  set.seed(391)
  
  attitude <-
    -0.26 * exposure + 0.15 * contact + 0.04 * exposure * contact + rnorm(n, mean = 0.4, sd = 0.9) 
  
  #Function used to plot regression line at specific contact value
  attfun <- function(exposure,contact){
    -0.26*exposure + 0.15*contact + 0.04*exposure*contact + 0.4
  }
  
  #PLOT OUTPUT
  output$mainplot <- renderPlot({
    #Helper data frame for plotting the regression lines
    df <- data.frame(x = c(0, 6))
    
    #Actual data
    scatter <- data.frame(attitude = attitude, exposure = exposure, contact = contact)
    
    
    #Basic plot used if nothing is selected
    p <- 
      ggplot(df, aes(x = x)) +
      geom_point(data = scatter,
                 shape = 21,
                 size = 3,
                 aes(x = exposure,
                     y = attitude)) + 
      coord_cartesian(xlim = c(0, 10), ylim = c(-5, 5)) +
      ylab("Attitude") +
      xlab("Exposure") +
      theme_general() + 
      theme(legend.position = "bottom")
    # Series of if/then statements adding elements to plot if checkbox is selected
    if(sum(input$checkboxes == "Minimum")) {
      #Manual coloring is necessary because ggplot does not support multiple continous colourscales.
      colormin <- 
        rgb(1,0,0,alpha = dnorm(scatter$contact, min(scatter$contact), .5))
      #Add points and regression line
      p <- 
        p + 
          geom_point(data = scatter,
                     shape = 21,
                     size = 3,
                     aes(x = exposure,
                         y = attitude),
                     fill = colormin) + 
        stat_function(
          fun = attfun,
          args = list(contact = min(contact)),
          n = 500,
          alpha = 1,
          size = .9,
          color = rgb(1,0,0))  
    }
    if(sum(input$checkboxes == "First tercile")) {
      ft <- quantile(scatter$contact,c(0.33))
      colorft <- 
        rgb(0.5,1,0,alpha = dnorm(scatter$contact, ft, .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colorft) + 
        stat_function(
          fun = attfun,
          args = list(contact = ft),
          n = 500,
          alpha = 1,
          size = .9,
          color = rgb(0.5,1,0)) 
    }
    if(sum(input$checkboxes == "Second tercile")) {
      st <- quantile(scatter$contact,c(0.66))
      colorst <- 
        rgb(0,0.5,1,alpha = dnorm(scatter$contact, st, .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colorst) + 
        stat_function(
          fun = attfun,
          args = list(contact = st),
          n = 500,
          alpha = 1,
          size = .9,
          color = rgb(0,0.5,1)) 
    }
   if(sum(input$checkboxes == "Mean - 2SD")) {
     
     m2sd <- mean(scatter$contact) - 2 * sd(scatter$contact)
     colorm2sd <- 
       rgb(1,.5,0,alpha = dnorm(scatter$contact, m2sd, .5))
     p <- 
       p + 
       geom_point(data = scatter,
                  shape = 21,
                  size = 3,
                  aes(x = exposure,
                      y = attitude),
                  fill = colorm2sd) + 
       stat_function(
         fun = attfun,
         args = list(contact = m2sd),
         n = 500,
         alpha = 1,
         size = .9,
         color = rgb(1,.5,0)) 
    }
    if(sum(input$checkboxes == "Mean - 1SD")) {
      m1sd <- mean(scatter$contact) - 1 * sd(scatter$contact)
      colorm1sd <- 
        rgb(.75,.75,0, alpha = dnorm(scatter$contact, m1sd, .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colorm1sd) + 
        stat_function(
          fun = attfun,
          args = list(contact = m1sd),
          n = 500,
          alpha = 1,
          size = .9,
          color = rgb(.75,.75,0)) 
    } 
    if(sum(input$checkboxes == "Mean")) {
      m <- mean(scatter$contact)
      colorm <- 
        rgb(0,1,1, alpha = dnorm(scatter$contact, m, .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colorm) + 
        stat_function(
          fun = attfun,
          args = list(contact = m),
          n = 500,
          alpha = 1,
          size = .9,
          color =rgb(0,1,1)) 
    }
    
    if(sum(input$checkboxes == "Mean + 1SD")) {
      mp1sd <- mean(scatter$contact) + 1 * sd(scatter$contact)
      colormp1sd <- 
        rgb(0,0,1, alpha = dnorm(scatter$contact, mp1sd, .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colormp1sd) + 
        stat_function(
          fun = attfun,
          args = list(contact = mp1sd),
          n = 500,
          alpha = 1,
          size = .9,
          color =  rgb(0,0,1)) 
    }
    if(sum(input$checkboxes == "Mean + 2SD")) {
      mp2sd <- mean(scatter$contact) + 2 * sd(scatter$contact)
      colormp2sd <- 
        rgb(.5,0,1,alpha = dnorm(scatter$contact, mp2sd, .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colormp2sd) + 
        stat_function(
          fun = attfun,
          args = list(contact = mp2sd),
          n = 500,
          alpha = 1,
          size = .9,
          color = rgb(.5,0,1)) 
    }
    if(sum(input$checkboxes == "Maximum")) {
      colormax <- 
        rgb(1,0,1,alpha = dnorm(scatter$contact, max(scatter$contact), .5))
      p <- 
        p + 
        geom_point(data = scatter,
                   shape = 21,
                   size = 3,
                   aes(x = exposure,
                       y = attitude),
                   fill = colormax) + 
        stat_function(
          fun = attfun,
          args = list(contact = max(contact)),
          n = 500,
          alpha = 1,
          size = .9,
          color = rgb(1,0,1)) 
    }
    #Print the plot
    p 
  })
  
})
