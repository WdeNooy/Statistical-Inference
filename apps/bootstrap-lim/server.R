library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

#Load general plot theme and colors for color brewer
source("../plottheme/styling.R")

#Function that returns coordinates of dots, so that they stack nicely
#on top of each other. 
dotcoordinates <- function(numberofdots, dotsinrow = 5, xscale = 1, yscale = 1){
  xtemp <- numeric()
  ytemp <- numeric()
  
  if(numberofdots == 0) {
    xtemp <- numeric(0)
    ytemp <- numeric(0)
  }
  if (numberofdots <= dotsinrow & numberofdots > 0) {
    xtemp <- seq(1, numberofdots)
    ytemp <- rep(1, numberofdots)
  }
  if (numberofdots > dotsinrow) {
    xtemp <- rep(c(1:dotsinrow), floor(( numberofdots / dotsinrow)))
    if((numberofdots %% dotsinrow) != 0){
      xtemp <- c(xtemp, 1:(numberofdots %% dotsinrow))
    }
    ytemp <- rep(1:floor(numberofdots / dotsinrow), each = dotsinrow)
    ytemp <-
      c(ytemp, rep(ceiling(numberofdots / dotsinrow), numberofdots %% dotsinrow))
  }
  xtemp <- xtemp * xscale
  ytemp <- ytemp * yscale
  return(cbind(xtemp,ytemp))
}

shinyServer(function(input, output) {
  
  repstheor <- 5000  #size of theoretical sample
  
  # reactive data container
  samples <- 
    reactiveValues(
      bootstrapsample = factor(),
      lastsample =  numeric()
    )
  
  #Sampling distribution
  theoreticalsample <- replicate(sample(1:5, size = 50,
                                        prob = rep(0.2,5),
                                        replace = TRUE),
                                 n = repstheor)
  #Sample from theoretical distribution
  theoreticalsample <-
    apply(X = theoreticalsample,
          MARGIN = 2,
          function(x) prop.table(table(x))["5"])
  theoreticalsample <- data.frame(prop = theoreticalsample)
  
  # Plot displaying the population and colors
  output$populationpropplot <-  renderPlot({
    
    candy <- list(
      proportions = rep(0.2, 5),
      colornames = c("Red", "Orange", "Yellow", "Green", "Blue")
    )
    
    ggplot(as.data.frame(candy),
           aes(x = colornames, y = proportions, fill = colornames)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        name = "Proportion",
        breaks = seq(0, 1, by = 0.2),
        limits = c(0, 1)
      ) +
      scale_x_discrete(name = "Candy" , breaks = candy$colornames) +
      scale_fill_manual(values = brewercolors) +
      ggtitle("Candy proportions in population") +
      theme_general() + 
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
  })
  
  output$sampleplot <- renderPlot({
    samples$lastsample <- sample(x = 1:5,
                                 size = input$samplesizeslider,
                                 replace = TRUE)
    sample <- factor(samples$lastsample,
                     levels = c(1:5),
                     labels = sort(c("Red",
                                     "Orange",
                                     "Yellow",
                                     "Green",
                                     "Blue"
                                     )
                                   )
                     )
    sample <- sort(sample)
    #Make coordinates for all five categories
    tempcoord <- numeric()
    coordinates <- numeric()
    #Generate dotcoordinates for each category
    for (i in 1:length(levels(sample))) {
      data <- sample[sample == levels(sample)[i]]
      tempcoord <- dotcoordinates(length(data), yscale = 2)
      tempcoord[, 1] <- tempcoord[, 1] + (((i - 1) * 6))
      coordinates <- rbind(coordinates, tempcoord)
    }
    
    df <- data.frame(sample,coordinates)
    #Generate plot
    ggplot(data = df, 
           aes(x = xtemp,
               y = ytemp,
               fill = sample)
           ) +
      geom_point(shape = 21,
                 size = 4,
                 color = "black"
                 ) +
      scale_fill_manual(values = brewercolors) +
      scale_x_continuous(
        name = "",
        breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
        limits = c(0, 30),
        labels = sort(names(brewercolors))
      ) +
      scale_y_continuous(name = "",
                         labels = c(),
                         limits = c(0, 15)
                         ) +
      ggtitle("Initial sample from population") + 
      theme_general() +
      theme(line = element_blank(),
            legend.position  = "none"
            )
  })
  
  output$samplingdistplot <- renderPlot({
    # replication of bootstrap samples - set below 5,000 for speed
    reps = 1000
    validate(
      need(samples$lastsample != "", "Can not sample from empty data set")
    )
    newsample <- replicate(sample(x = samples$lastsample,
                                  size = 50,
                                  replace = TRUE
                                  ),
                           n = reps
                          )
    
    newprop <- apply(X = newsample,
                    MARGIN = 2,
                    function(x) prop.table(table(x))["5"]
                    )
    newprop[is.na(newprop)] <- 0
    
    df <- data.frame(prop = newprop)
    
    ggplot(df, aes(x = prop)) + 
      geom_histogram(color = "Black",
                     fill = "Grey",
                     alpha = .4,
                     data = theoreticalsample,
                     bins = 30,
                     aes(x = prop,
                         y = ..count../sum(..count..)
                     ))+
      geom_histogram(fill = brewercolors["Yellow"],
                     color = "Grey",
                     alpha = .6,
                     bins = 30,
                     aes(y = ..count../sum(..count..))) + 
      ggtitle("5,000 bootstrap samples from the initial sample") +
      scale_x_continuous(name = "Proportion of yellow candies",
                         limits = c(-0.2,1))+
      scale_y_continuous(name = "Probability",
                         limits = c(0,0.5),
                         breaks = seq(0,1,by = 0.2))+
      theme_general()
  })
})
