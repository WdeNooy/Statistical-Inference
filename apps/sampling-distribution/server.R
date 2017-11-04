library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

#Load general plot theme and colors for color brewer
source("../plottheme/styling.R",local = TRUE)

shinyServer(function(input, output) {
  # Handy candy variables
  candy <- list(
    proportions = rep(0.2, 5), #c(0.15, 0.15, 0.3, 0.2, 0.2),
    colornames = c("Red", "Orange", "Yellow", "Green", "Blue")
  )
  

  #Container for all samples taken
  candy.sample.history.df <- character()
  
  #Plot of proportions
  output$populationproportions <-  renderPlot({
    ggplot(as.data.frame(candy),
           aes(x = colornames, y = proportions, fill = colornames)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        name = "Proportion",
        breaks = seq(0, 1, by = 0.2),
        limits = c(0, 1)
      ) +
      scale_x_discrete(name = "Candy color" , breaks = candy$colornames) +
      scale_fill_manual(values = brewercolors) +
      ggtitle("Candy proportions in the population") +
      theme_general() + 
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
  })
  
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
    candy.sample.df <-
      data.frame(candy.sample = factor(
        sample(
          candy$colornames,
          prob = candy$proportions,
          replace = TRUE,
          size = 10
        ),
        levels = sort(candy$colornames)
      ))
    #Append sample to history of samples
    candy.sample.history.df <<-
      rbind(candy.sample.history.df,
            as.data.frame(
              table(candy.sample.df$candy.sample,
                    exclude = candy$colornames[candy$colornames != "Yellow"])
            ))
    
    #Dotplot small sample
    output$countplot <- renderPlot({
      ggplot(candy.sample.df) +
        geom_dotplot(
          mapping = aes(candy.sample, fill = candy.sample),
          method = "dotdensity",
          dotsize = 2
        ) +
        scale_fill_manual(values = brewercolors,
                          limits = candy$colornames) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        scale_x_discrete(name = "Candy color",
                         breaks = candy$colornames,
                         drop = FALSE) +
        ggtitle("Last sample") + 
        theme_general() + 
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    })
    
    #Text indicating the yellow candies in last sample
    output$lastsampletext <-
      renderText(as.character(last(candy.sample.history.df$Freq)
      ))
    #Sampling-distribution plot small sample
    output$samplingstatisticplot <- renderPlot({
      
      #Highlighting the last sample tickmark to red.
      tickmarkcolors <- rep("black", 11)
      tickmarkcolors[tail(candy.sample.history.df$Freq, n= 1) + 1] <- "red"
      
      ggplot(candy.sample.history.df, aes(x = Freq)) +
        geom_bar(fill = brewercolors["Yellow"]) +
        scale_x_continuous(name = "Number of yellow candies",
                           breaks = 0:10,
                           limits = c(-1, 10)) +
        scale_y_continuous(
          breaks =  function (x)
            floor(pretty(seq(1, max(x) + 1)))) +
        ggtitle("Sampling distribution") +
        theme_general() + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x= element_text(colour = tickmarkcolors)) 
        
    })
    
    # Limit sample size to 3 Mb
    if(object.size(candy.sample.history.df) > 3e+06) {
      candy.sample.history.df <<- character()
    }
    
  })
  
  #Events triggered by large sample button
  observeEvent(input$largesample, {
    
    for (i in 1:1000) {
      candy.sample.df <-
        data.frame(candy.sample = factor(
          sample(
            candy$colornames,
            prob = candy$proportions,
            replace = TRUE,
            size = 10
          ),
          levels = sort(candy$colornames)
        ))
      #Append 
      candy.sample.history.df <<-
        rbind(candy.sample.history.df,
              as.data.frame(
                table(candy.sample.df$candy.sample,
                      exclude = candy$colornames[candy$colornames != "Yellow"])
              ))
      
      # Limit sample size to 3 Mb
      if(object.size(candy.sample.history.df) > 3e+06) {
        candy.sample.history.df <<- character()
      }
    }
    
    
    
    #Dotplot large sample
    output$countplot <- renderPlot({
      ggplot(candy.sample.df) +
        geom_dotplot(
          mapping = aes(candy.sample, fill = candy.sample),
          method = "dotdensity",
          dotsize = 2
        ) +
        scale_fill_manual(values = brewercolors,
                          limits = candy$colornames) +
        scale_y_continuous(name = NULL, breaks = NULL) +
        scale_x_discrete(name = "Candy color",
                         breaks = candy$colornames,
                         drop = FALSE) +
        ggtitle("Last sample") +
        theme_general() + 
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    })
    
    #Text indicating the yellow candies in last sample
    output$lastsampletext <-
      renderText(as.character(last(candy.sample.history.df$Freq)))
      
    
    #Sampling-distribution plot large sample
    output$samplingstatisticplot <- renderPlot({
      
      #Highlighting the last sample tickmark to red.
      tickmarkcolors <- rep("black", 11)
      tickmarkcolors[tail(candy.sample.history.df$Freq, n= 1) + 1] <- "red"
      
      ggplot(candy.sample.history.df, aes(x = Freq)) +
        geom_bar(fill = brewercolors["Yellow"]) +
        scale_x_continuous(name = "Number of yellow candies",
                           breaks = 0:10,
                           limits = c(-1, 10)) +
        scale_y_continuous(
          breaks =  function (x) floor(pretty(seq(1, max(x) + 1)))) +
        ggtitle("Sampling distribution") +
        theme_general() + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x= element_text(colour = tickmarkcolors))
    })
  })
  #Reset button
  observeEvent(input$reset,{
    candy.sample.history.df <<- character()
    
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
