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
  reps = 1000 # number of of repetitions for large bootstrap

 # Reactive container for changing values
  samples = reactiveValues(
      firstsample = rep(1:5, each = 5),
      firstsampleID = c(1:25),
      hist = numeric(),
      lastsampleID = numeric(),
      lastsample = numeric()
    )
  
  # Reset to initial sample
  observeEvent(input$resetsampleaction,{
    samples$firstsample <<- rep(1:5, each = 5)
    samples$firstsampleID <<- c(1:25) 
    samples$hist = numeric()
    samples$lastsample = numeric()
    samples$lastsampleID = numeric()
  })
  
  # When new initial sample is taken, take sample, clear history.
  observeEvent(input$firstsampleaction,{
    samples$firstsample <<- sort(sample(1:5, size = 25, replace = TRUE))
    samples$firstsample <<- sort(samples$firstsample)
    samples$firstsampleID <<- c(1:25) 
    samples$hist = numeric()
    samples$lastsample = numeric()
    samples$lastsampleID = numeric()
  })
  
  # When single bootstrap sample is taken, take sample, append to history
  observeEvent(input$bootstrapsmallaction, { 
               
       # draw sample of rownumbers 
       newsample = sort(sample(samples$firstsampleID, 25, replace = TRUE))
       
       # select values rows from new initial sample 
       samples$lastsample <<- samples$firstsample[newsample]
       # select row numbers from new initial sample 
       samples$lastsampleID <<- newsample
      
       # calculate proportion of yellow candies in bootstrap 
       newprop = (mean(samples$lastsample == 5, na.rm = T))
       samples$hist <<- c(samples$hist, newprop)
                                     
       # Limit size of samples to 3 Mb
       if(object.size(samples) > 3e+06) {
       samples$firstsample <<- sample(1:5, size = N, replace = TRUE)
       samples$hist <<- numeric()
       samples$lastsample <<- numeric()}
      
       })
  
  # When big sample is taken, take sample, append to history, store last sample
  observeEvent(input$bootstraplargeaction, {
       # initial sample 
       oldsample = samples$firstsample
       
       # draw 1000 bootstraps
       newsample = replicate(sample(oldsample, 25, replace = TRUE), n = reps)
       
       # create artificial last sample 
       newsample2 = sort(sample(samples$firstsampleID, 25, replace = TRUE))
       samples$lastsample <<- samples$firstsample[newsample2]
       samples$lastsampleID <<- newsample2
       
       # calculate proportion of yellow candies 
       newprop = apply(X = newsample, MARGIN = 2, function(x) 
         prop.table(table(x))["5"])
         newprop[is.na(newprop)] = 0
         samples$hist <<- c(samples$hist, newprop)
                                    
       # Limit size of samples to 3 Mb
       if(object.size(samples) > 3e+06) {
         samples$firstsample <<- sort(sample(1:5, size = 25, replace = TRUE))
         samples$hist <<- numeric()
         samples$lastsample <<- numeric()}
     })
  
  # PLOT OUTPUTS 
  
      # Plot 1: Sample 
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
          ggtitle("Original Sample") + 
          theme_general() + 
          theme(line = element_blank(),
                legend.position  = "none")})
  
    # Plot 2: One Bootstrap
    output$bootstrappedplot = renderPlot({
    
        # Store last sample 
        sample = sort(samples$lastsample)
        sampleID  = sort(samples$lastsampleID)
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
        ggtitle("Bootstrap Sample") + 
        theme_general() +
        theme(line = element_blank(),
              legend.position  = "none")})
    
  # Plot 3: Sampling Distribution
    output$sampdistplot <- renderPlot({
      df <- data.frame(prop = samples$hist)
      ggplot(df, aes(x = prop)) + 
        geom_col(data = data.frame(x = (0:10)/25, y = dbinom(0:10, 25, 0.2)),
                 aes(x, y), 
                 fill = "Grey",
                 color = "Black",
                 alpha = .4,
                 width = .04) +
        geom_histogram(fill = brewercolors["Yellow"],
                       color = "Grey",
                       alpha = .6,
                       binwidth = .04,
                       aes(y = ..count../sum(..count..))) + 
        ggtitle("Prop. Yellow Candies Bootstraps") +
        coord_cartesian(xlim = c(0, 0.4)) +
        scale_y_continuous(name = "", breaks = seq(0, 1 ,0.2), labels = seq(0, 1, 0.2),limits = c(0,1)) + 
        labs(x = "Proportion of yellow candies", y = "Probability") +
        theme_general() 
      })
})
