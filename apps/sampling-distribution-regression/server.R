library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

#Load general plot theme and colors for color brewer
source("../plottheme/styling.R",local = TRUE)

#Group size (2 groups, each of this size)
n = 12

shinyServer(function(input, output) {

  #Container for all samples taken
  sample.history.df <- character()
  
  #Clearing the stage
  output$lastsampletext <-renderText("")
  
  output$countplot <- renderPlot({
    ggplot() + 
      theme_general()
  })
  
  output$samplingstatisticplot <- renderPlot({
    ggplot() + 
      theme_general()
  })

  #Events triggered by small sample button
  observeEvent(input$smallsample, {
    #Take sample of candies
    Group <- c(rep("Smoker",n), rep("Non-smoker",n)) #Generate group identifier.
    smoker <- ifelse(Group == "Smoker", 1, 0) #Generate dummies.
    Exposure <- runif(2*n, min = 0.1, max = 9.8) #Generate random exposure scores between 0 and 10 for all observations.
    #Generate attitude outcome with or without Group interactions.
    #With different means for different groups.
    Attitude <- -0.7 * Exposure + #general negative effect
      0.5 * smoker + #average group differences
      0.6 * smoker * Exposure + #interaction smokers
      rnorm(n = 2*n, mean = 0, sd = 1) #random error
    #Rescale Attitude to [-5, 5]
    Attitude <- ((Attitude - min(Attitude)) / ((max(Attitude) - min(Attitude)) / 10)) - 5
    #Collect in dataframe
    df <- data.frame(Exposure, Attitude, Group)
  
    #Append regression coefficient of sample to history of samples
    sample.history.df <<-
      rbind(sample.history.df,
            data.frame(
              exposure = lm(Attitude ~ Exposure)$coef[[2]]
            ))
    
    #Dotplot small sample
    output$countplot <- renderPlot({
      ggplot(df, aes(x = Exposure, y = Attitude)) +
        geom_point(aes(colour = Group), size = 2) +
        geom_smooth(method="lm", fill=NA, n = 1000, fullrange = FALSE) +
        geom_text(aes(x = 10, y = 5, hjust = 1, vjust = 1,
                      label = paste0("b = ", round(last(sample.history.df$exposure), digits = 2))),
                  color = "Red", size = 8) +
        scale_colour_manual(values = c("Smoker" = unname(brewercolors["Green"]),
                                       "Non-smoker" = unname(brewercolors["Red"]))) +
        ggtitle("Sample") +
        theme_classic(base_size = 16) +
        theme(legend.position = c(0,0), legend.justification = c(0,0), legend.background = element_blank(),
              panel.border=element_rect(fill=NA), plot.title = element_text(hjust = 0.5)) +
        xlim(c(0,10)) +
        ylim(c(-5, 5))
      })
      
    #Sampling-distribution plot small sample
    output$samplingstatisticplot <- renderPlot({
      
      ggplot(sample.history.df, aes(x = exposure)) +
        geom_histogram(binwidth = 0.1, fill = brewercolors["Blue"]) +
        geom_histogram(data = tail(sample.history.df, n= 1), aes(x = exposure), binwidth = 0.1, fill = "Red") +
        geom_label(aes(x = 0, y = 0.1, vjust = 0,
                       label = paste0("M = ", round(mean(sample.history.df$exposure), digits = 2),
                                      "\nSD = ", round(sd(sample.history.df$exposure), digits = 2)))) +
        scale_x_continuous(name = "Regression coefficient", 
                           breaks = round(seq(from = -1.2, to = 0.2, by = 0.1), digits = 1)) +
        scale_y_continuous(
          breaks =  function (x)
            floor(pretty(seq(1, max(x) + 1)))) +
        ggtitle("Sampling distribution") +
        theme_classic(base_size = 16) +
        theme(panel.border=element_rect(fill=NA), plot.title = element_text(hjust = 0.5))
        
        
    })
    
    # Limit sample size to 3 Mb
    if(object.size(sample.history.df) > 3e+06) {
      sample.history.df <<- character()
    }
    
  })
  
  #Events triggered by large sample button
  observeEvent(input$largesample, {
    
      #Append 
      sample.history.df <<-
        rbind(sample.history.df,
              data.frame(exposure = rnorm(500, mean = -0.47, sd = 0.19))
              )
      
      # Limit sample size to 3 Mb
      if(object.size(sample.history.df) > 3e+06) {
        sample.history.df <<- character()
      }
    
    #Sampling-distribution plot large sample
    output$samplingstatisticplot <- renderPlot({
      
      ggplot(sample.history.df, aes(x = exposure)) +
        geom_histogram(binwidth = 0.1, fill = brewercolors["Blue"]) +
        geom_label(aes(x = 0, y = 0.1, vjust = 0,
                       label = paste0("M = ", round(mean(sample.history.df$exposure), digits = 2),
                                      "\nSD = ", round(sd(sample.history.df$exposure), digits = 2)))) +
        scale_x_continuous(name = "Regression coefficient", 
                           breaks = round(seq(from = -1.2, to = 0.2, by = 0.1), digits = 1)) +
        scale_y_continuous(
          breaks =  function (x)
            floor(pretty(seq(1, max(x) + 1)))) +
        ggtitle("Sampling distribution") +
        theme_classic(base_size = 16) +
        theme(panel.border=element_rect(fill=NA), plot.title = element_text(hjust = 0.5))
    })
  })
  
  #Reset button
  observeEvent(input$reset,{
    sample.history.df <<- character()
    
    output$lastsampletext <-renderText("")
    
    output$countplot <- renderPlot({
      ggplot() + 
        theme_general()
    })
    
    output$samplingstatisticplot <- renderPlot({
      ggplot() + 
        theme_general()
    })
  })
})
