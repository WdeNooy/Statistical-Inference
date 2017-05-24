library(shiny)
library(ggplot2)

#load colorpalette & styling for plots
source("../plottheme/styling.R",local = TRUE)

shinyServer(function(input, output) {
    #Handy candy variables
    candy <- list(
    colornames = c("Red", "Orange", "Yellow", "Green", "Blue"),
    counts = c(30, 30, 30, 30,30)
  )
  
  #X variable necessary to generate nice looking population plot
  x <- 1:29  
  x <- x[x%%6 != 0]
  
  #Generate data frame for population plot
  candy.coordinates.df = data.frame(
    candy.colornames = factor(rep(candy$colornames, each = 5, times = 150/25), levels = candy$colornames),
    x = rep(x, 6),
    y = rep(seq(0,2, length.out = 6), each = 25)
  )
  #Plot populations
  output$populatieplot <- renderPlot({
    ggplot(candy.coordinates.df, aes(x = x, y = y, color = candy.colornames)) +
      geom_point(size =5) +
      scale_color_manual(values = brewercolors) +
      ylim(-0.5,3)+
      theme(
        rect             = element_blank(),
        line             = element_blank(),
        text             = element_blank(),
        legend.position  = "none"
      )
    
  })
  
  observeEvent(input$smallsample, {
    candy.sample.coordinates.df <-
      candy.coordinates.df[sample(dim(candy.coordinates.df)[1],
                                  size = 10,
                                  replace = FALSE), ]
    
    output$populatieplot <- renderPlot({
      ggplot(candy.coordinates.df, aes(x = x, y = y, color = candy.colornames)) +
        geom_point(size = 5) +
        ylim(-0.5,3)+
        scale_color_manual(values = brewercolors) +
        geom_point(
          data = candy.sample.coordinates.df,
          size = 7,
          colour = "black",
          shape = "O",
          aes(x = x, y = y)
        ) +
        theme_general() +
        theme(
          rect             = element_blank(),
          line             = element_blank(),
          text             = element_blank(),
          legend.position  = "none"
        )
      
      
    })
    
    output$countplot <- renderPlot({
      ggplot(candy.sample.coordinates.df,
             aes(x = candy.colornames, fill = candy.colornames)) +
        geom_dotplot(method = "dotdensity", dotsize = 2) +
        scale_fill_manual(
          values = brewercolors,
          limits = candy$colornames) +
          scale_y_continuous(name = NULL, breaks = NULL) +
        scale_x_discrete(name = "Candy color", breaks = candy$colornames, drop = FALSE) +
        theme_general() +
        theme(legend.position = "none")
        
    })
    
  })
  
  
})
