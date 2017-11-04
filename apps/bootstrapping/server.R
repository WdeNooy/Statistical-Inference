library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

#Load general plot theme and colors for color brewer
source("../plottheme/styling.R",local = TRUE)

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

  N <- 50 #size of a single sample
  reps <- 1000 #number of of repetitions for large bootstrap
  repstheor <- 5000  #size of theoretical sample

  #Sampling distribution
  theoreticalsample <- replicate(sample(1:5, size = N,
                                        prob = rep(0.2,5),
                                        replace = TRUE),
                                 n = repstheor)
  #Generate proportions of for each of the samples
  theoreticalsample <-
    apply(X = theoreticalsample,
          MARGIN = 2,
          function(x) prop.table(table(x))["5"])
  theoreticalsample <- data.frame(prop = theoreticalsample)

 # Reactive container for changing values
  samples <- 
    reactiveValues(
      firstsample = rep(1:5,each = 10),
      hist = factor(),
      lastsample = factor()
    )
  
  # When initial sample is taken, take sample, clear history.
  observeEvent(input$firstsampleaction,{
    samples$firstsample <- sample(1:5, size = 50, replace = TRUE)
    samples$hist <- numeric()
    samples$lastsample <- numeric()
  })
  
  # When single sample is taken, take sample, append to history.
  observeEvent(input$bootstrapsmallaction,{
                                     newsample <-
                                       replicate(sample(x = samples$firstsample,
                                              size = N,
                                              replace = TRUE), n = 1)
                                     samples$lastsample <<- newsample
                                     newprop <-
                                       prop.table(table(newsample))["5"]
                                     newprop[is.na(newprop)] <- 0
                                     samples$hist <<- c(samples$hist, newprop)
                                     
                                     # Limit size of sample to 3 Mb
                                     if(object.size(samples) > 3e+06) {
                                       samples$firstsample <<- sample(1:5, size = 50, replace = TRUE)
                                       samples$hist <<- numeric()
                                       samples$lastsample <<- numeric()
                                     }
                                     
                                   })
  
  # When big sample is taken, take sample, append to history, store last sample.
  
  observeEvent(input$bootstraplargeaction,
                                   {
                                    newsample <-
                                     replicate(sample(x = samples$firstsample,
                                            size = N,
                                            replace = TRUE),
                                            n = reps)
                                    samples$lastsample <<- 
                                      newsample[ , reps]
                                    newprop <-
                                      apply(X = newsample,
                                            MARGIN = 2,
                                            function(x) prop.table(table(x))["5"])
                                    newprop[is.na(newprop)] <- 0
                                    samples$hist <<- c(samples$hist, newprop)
                                   # Limit size of sample to 3 Mb
                                     if(object.size(samples) > 3e+06) {
                                      samples$firstsample <<- sample(1:5, size = 50, replace = TRUE)
                                      samples$hist <<- numeric()
                                      samples$lastsample <<- numeric()
                                    }
                                   })
  
  # Render Plot of last sample.
  
  output$sampleplot <- renderPlot({

    # Store sample and make factor.
    sample <- samples$firstsample
    sample <- factor(sample,
              levels = c(1:5),
              labels = sort(c("Red",
                             "Orange",
                             "Yellow",
                             "Green",
                             "Blue")))
    #Sort before passing to generate dotcoordinates
    sample <- sort(sample)
    
    #Make coordinates for all five categories
    tempcoord <- numeric()
    coordinates <- numeric()
    for (i in 1:length(levels(sample))) {
      data <- sample[sample == levels(sample)[i]]
      tempcoord <- dotcoordinates(length(data),yscale = 2)
      tempcoord[, 1] <- tempcoord[, 1] + (((i - 1) * 6))
      coordinates <- rbind(coordinates, tempcoord)
    }
    
    # Store dotcoordinates in data frame
    df <- data.frame(sample,coordinates)
    
    # Generate plot
    ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample)) +
      geom_point(shape = 21, size = 4, color = "black") +
      scale_fill_manual(values = brewercolors) +
      scale_x_continuous(
        name = "",
        breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
        limits = c(0, 30),
        labels = sort(names(brewercolors))
      ) +
      scale_y_continuous(name = "",
                         labels = c(),
                         limits = c(0, 15)) +
      ggtitle("Initial Sample") + 
      theme_general() + 
      theme(line = element_blank(),
            legend.position  = "none")
  })
  
  # Render bootstrapped examples
  output$bootstrappedplot <- renderPlot({
    # Store last sampleS  
    sample <- samples$lastsample
    sample <- factor(sample,
                     levels = c(1:5),
                     labels = sort(c("Red",
                                     "Orange",
                                     "Yellow",
                                     "Green",
                                     "Blue")))
    #Sort before passing to generate dotcoordinates
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
      ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample)) +
        geom_point(shape = 21,
                   size = 4,
                   color = "black") +
        scale_fill_manual(values = brewercolors) +
        scale_x_continuous(
          name = "",
          breaks = c(2.5, 8.5, 15.5, 21.5, 28.5),
          limits = c(0, 30),
          labels = sort(names(brewercolors))
        ) +
        scale_y_continuous(name = "",
                           labels = c(),
                           limits = c(0, 15)) +
        ggtitle("Last drawn sample") + 
        theme_general() +
        theme(line = element_blank(),
              legend.position  = "none")
  })
  
  # Render plot of distributions.
  output$sampdistplot <- renderPlot({
    # Store hist in data frame.  
    df <- data.frame(prop = samples$hist)
    
      ggplot(df, aes(x = prop)) + 
        geom_histogram(color = "Black",
                       fill = "Grey",
                       alpha = .4,
                       data = theoreticalsample,
                       binwidth = .02,
                       aes(x = prop,
                           y = ..count../sum(..count..)
                       ))+
      geom_histogram(fill = brewercolors["Yellow"],
                     color = "Grey",
                     alpha = .6,
                     binwidth = .02,
                     aes(y = ..count../sum(..count..))) + 
      ggtitle("Proportions of yellow candies in all samples") +
      scale_x_continuous(name = "Proportion of yellow candies",
                        limits = c(0,0.45),
                        breaks = seq(0, 0.45, by = 0.05))+
        scale_y_continuous(name = "Density",
                           limits = c(0,1.1),
                           breaks = seq(0,1,by = 0.2))+
      theme_general()
  })

})
