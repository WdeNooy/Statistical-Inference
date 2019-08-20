library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

# CORRECTED VERSION

# Load general plot theme and colors for color brewer
source("../plottheme/styling.R",local = TRUE)

# Function that returns coordinates of dots, so that they stack nicely
# on top of each other. 
dotcoordinates = 
  function(numberofdots, dotsinrow = 1, xscale = 1, yscale = 1){
    xtemp = numeric()
    ytemp = numeric()
  
  if(numberofdots == 0) {
    xtemp = numeric(0)
    ytemp = numeric(0)}
    
  if (numberofdots <= dotsinrow & numberofdots > 0) {
    xtemp = seq(1, numberofdots)
    ytemp = rep(1, numberofdots)}
    
  if (numberofdots > dotsinrow) {
    xtemp = rep(c(1:dotsinrow), floor(( numberofdots / dotsinrow)))
    if((numberofdots %% dotsinrow) != 0){
      xtemp = c(xtemp, 1:(numberofdots %% dotsinrow))}
      ytemp = rep(1:floor(numberofdots / dotsinrow), each = dotsinrow)
      ytemp = c(ytemp, rep(ceiling(numberofdots / dotsinrow), numberofdots %% dotsinrow))}
  xtemp = xtemp * xscale
  ytemp = ytemp * yscale
  
  return(cbind(xtemp,ytemp))}

shinyServer(function(input, output) {

  N = 25 # size of a single sample

 # Reactive container for changing values
  samples = reactiveValues(
      #original sample
      firstsample = rep(1:5, each = 5),
      firstsampleID = c(1:25),
      #copy of original sample (no replacement)
      copysampleID = numeric(),
      copysample = numeric(),
      #new sample with replacement
      newsampleID = numeric(),
      newsample = numeric()
    )
  
  # When new initial sample is taken, take sample, clear history.
  observeEvent(input$firstsampleaction,{
    samples$firstsample <<- sort(sample(1:5, size = 25, replace = TRUE))
    samples$firstsample <<- sort(samples$firstsample)
    samples$firstsampleID <<- c(1:25)
    samples$copysampleID = numeric()
    samples$copysample = numeric()
    samples$newsample = numeric()
    samples$newsampleID = numeric()
  })
  
  # Draw a single bootstrap without replacement.
  observeEvent(input$bootstrapsmallaction, { 
               
       # just copy the original sample 
       newsample = samples$firstsampleID #sort(sample(samples$firstsampleID, 25, replace = TRUE))
       
       # select values rows from new initial sample
       samples$copysample <<- samples$firstsample[newsample]
       # select row numbers from new initial sample
       samples$copysampleID <<- newsample

       })
  
  # Draw a single bootstrap with replacement.
  observeEvent(input$bootstraplargeaction, { 
    
    # draw sample of rownumbers 
    newsample = sort(sample(samples$firstsampleID, 25, replace = TRUE))
    
    # select values rows from new initial sample 
    samples$newsample <<- samples$firstsample[newsample]
    # select row numbers from new initial sample 
    samples$newsampleID <<- newsample
    
  })
  
  
  # PLOT OUTPUTS 
  
      # Plot 1: Original Sample 
      output$sampleplot = renderPlot({
            
        # Store sample and sampleID 
        sample = sort(samples$firstsample)
        sampleID = sort(samples$firstsampleID)
        sample = factor(sample, levels = c(1:5), 
                        labels = sort(c("Red", "Orange", "Yellow", "Green", "Blue")))
    
        # Make coordinates for all five categories
        tempcoord = numeric()
        coordinates = numeric()
        for (i in 1:length(levels(sample))) {
          data = sample[sample == levels(sample)[i]]
          tempcoord = dotcoordinates(length(data),yscale = 2)
          tempcoord[, 1] = tempcoord[, 1] + (((i - 1) * 6))
          coordinates = rbind(coordinates, tempcoord)
        }
    
        # Dataframe of sample, sample ID and coordinates 
        df = data.frame(sample, sampleID, coordinates)
    
        # calculates proportion of yellow candies in bootstrap sample
        yellow = nrow(df[df$sample == "Yellow",])
        yellow = as.character(format(round(yellow / 25, digits = 2), nsmall = 2))
        
        # Generate plot
        ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample, label = sampleID)) +
          geom_point(shape = 21, size = 8, color = "black") +
          geom_text(size = 3) + 
          scale_fill_manual(values = brewercolors) +
          scale_x_continuous(name = "", 
            breaks = c(1, 7, 13, 19, 25),
            limits = c(-2, 29),
            labels = sort(names(brewercolors))) +
          scale_y_continuous(name = "", labels = c(), limits = c(1.5, 25)) +
          geom_label(aes(x=1, y = 24.90, hjust = 0), label = paste("Proportion Yellow:", yellow), 
                     color = "white", alpha=0.7, fontface = "bold", fill = "#C99800") + 
          ggtitle("Original Sample") + 
          theme_general() + 
          theme(line = element_blank(),
                legend.position  = "none")})
  
    # Plot 2: Draw Sample Without Replacement. Same as original plot.
      output$noreplacementplot = renderPlot({
        
        # Store sample and sampleID 
        sample = sort(samples$copysample)
        sampleID = sort(samples$copysampleID)
        sample = factor(sample, levels = c(1:5), 
                        labels = sort(c("Red", "Orange", "Yellow", "Green", "Blue")))
        
        # Make coordinates for all five categories
        tempcoord = numeric()
        coordinates = numeric()
        for (i in 1:length(levels(sample))) {
          data = sample[sample == levels(sample)[i]]
          tempcoord = dotcoordinates(length(data),yscale = 2)
          tempcoord[, 1] = tempcoord[, 1] + (((i - 1) * 6))
          coordinates = rbind(coordinates, tempcoord)
        }
        
        # Dataframe of sample, sample ID and coordinates 
        df = data.frame(sample, sampleID, coordinates)
        
        # calculates proportion of yellow candies in bootstrap sample
        yellow = nrow(df[df$sample == "Yellow",])
        yellow = as.character(format(round(yellow / 25, digits = 2), nsmall = 2))
        
        # Generate plot
        ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample, label = sampleID)) +
          geom_point(shape = 21, size = 8, color = "black") +
          geom_text(size = 3) + 
          scale_fill_manual(values = brewercolors) +
          scale_x_continuous(name = "", 
                             breaks = c(1, 7, 13, 19, 25),
                             limits = c(-2, 29),
                             labels = sort(names(brewercolors))) +
          scale_y_continuous(name = "", labels = c(), limits = c(1.5, 25)) +
          geom_label(aes(x=1, y = 24.90, hjust = 0), label = paste("Proportion Yellow:", yellow), 
                     color = "white", alpha=0.7, fontface = "bold", fill = "#C99800") + 
          ggtitle("Sample Without Replacement") + 
          theme_general() + 
          theme(line = element_blank(),
                legend.position  = "none")})
      
    # Plot 3: Draw Sample With Replacement
    output$replacementplot = renderPlot({
      
      # Store last sample 
      sample = sort(samples$newsample)
      sampleID  = sort(samples$newsampleID)
      sample = factor(sample, levels = c(1:5),
                      labels = sort(c("Red", "Orange", "Yellow", "Green", "Blue")))
      
      #Make coordinates for all five categories
      tempcoord = numeric()
      coordinates = numeric()
      for (i in 1:length(levels(sample))) {
        data = sample[sample == levels(sample)[i]]
        tempcoord = dotcoordinates(length(data), yscale = 2)
        tempcoord[, 1] = tempcoord[, 1] + (((i - 1) * 6))
        coordinates = rbind(coordinates, tempcoord)}
      
      df = data.frame(sample, sampleID, coordinates)
      
      # calculates proportion of yellow candies in bootstrap sample
      yellow = nrow(df[df$sample == "Yellow",])  #plyr::count(subset(df, sample == "Yellow"), vars = "sample")
      yellow = as.character(format(round(yellow / 25, digits = 2), nsmall = 2))
      
      #Generate plot
      ggplot(data = df, aes(x = xtemp, y = ytemp, fill = sample, label = sampleID)) +
        geom_point(shape = 21, size = 8, color = "black") +
        geom_text(size = 3) + 
        scale_fill_manual(values = brewercolors) +
        scale_x_continuous(
          name = "",
          breaks = c(1, 7, 13, 19, 25),
          limits = c(-2, 29),
          labels = sort(names(brewercolors))) +
        scale_y_continuous(name = "", labels = c(), limits = c(1.5, 25)) +
        geom_label(aes(x=1, y = 24.90, hjust = 0), label = paste("Proportion Yellow:", yellow), 
                   color = "white", alpha=0.7, fontface = "bold", fill = "#C99800") + 
        ggtitle("Sample With Replacement") + 
        theme_general() +
        theme(line = element_blank(),
              legend.position  = "none")})
    
})
