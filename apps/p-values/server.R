library(ggplot2)
library(shiny)
library(RColorBrewer)


shinyServer(function(input, output) {
   
  #load standard colors
  source("../plottheme/styling.R",local = TRUE)
  #setting
  m = 2.8
  
  #Generate data for plotting
  df <- data.frame(x = seq(0, 6, by = 0.01),
                   prob.dens = dnorm(
                     x = seq(0, 6, by = 0.01),
                     mean = m,
                     sd = 0.6
                   ))
  
  #Plot the distribution
  output$pvalueplot <- renderPlot({
    
    #Prepare the data frames for plotting the probability polygons
    shadeleft  <-
      rbind(subset(df, x <= input$rangeslider[1]), c(input$rangeslider[1], 0))
    
    shaderight <-
      rbind(
        c(input$rangeslider[2], 0),
        subset(df, x >= input$rangeslider[2]),
        c(input$rangeslider[2], 0)
      )
    
    shadecenter <-
      rbind(
        c(input$rangeslider[1], 0),
        subset(df, x >= input$rangeslider[1] &
                 x <= input$rangeslider[2]),
        c(input$rangeslider[2], 0)
      )
    
    #Reset names to avoid empty subset breaking ggplot
    names(shadecenter) <- c("x", "prob.dens")
    # Main plot
    ggplot(data = data.frame(x = c(0, 6)), aes(x = x)) +
      geom_polygon(data = shadeleft, aes(x, y = prob.dens), fill = brewercolors["Red"]) +
      geom_polygon(data = shaderight, aes(x, y = prob.dens), fill = brewercolors["Green"]) +
      geom_polygon(data = shadecenter, aes(x, y = prob.dens), fill = brewercolors["Blue"]) +
      stat_function(fun = dnorm,
                    n = 101,
                    args = list(mean = m, sd = 0.6)) + ylab("Probability density") +
      xlab("") + 
      ggtitle("Sampling Distribution") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
    geom_label(
        data = data.frame(x = 0.8, y = 0.6),
        aes(x, y),
        colour = "white",
        fill = brewercolors["Red"],
        label = paste("Probability:", format(round(
          pnorm(input$rangeslider[1], mean = m, sd = 0.6), digits = 3
        ), nsmall = 3
        ))
      ) +
      geom_label(
        data = data.frame(x = 0.8, y = 0.4),
        aes(x, y),
        colour = "white",
        fill = brewercolors["Blue"],
        label = paste("Probability:", format(round((
          1 - pnorm(
            input$rangeslider[2],
            mean = m,
            sd = .6,
            lower.tail = FALSE
          ) - pnorm(
            input$rangeslider[1],
            mean = m,
            sd = 0.6,
            lower.tail = TRUE
          )
        ),
        digits = 3), nsmall = 3
        ))
      ) +
      geom_label(
        data = data.frame(x = 0.8, y = 0.2),
        aes(x, y),
        colour = "white",
        fill = brewercolors["Green"],
        label = paste("Probability:", format(round(
          pnorm(
            input$rangeslider[2],
            mean = m,
            sd = 0.6,
            lower.tail = FALSE
          ),
          digits = 3), nsmall = 3
        ))
      ) 
    
    
  })

})
