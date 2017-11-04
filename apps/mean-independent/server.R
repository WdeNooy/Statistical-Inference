library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Load general plot theme and colors for color brewer
  source("../plottheme/styling.R",local = TRUE)
  
  N <- 200 #population size
  redmean <- 2.8 # population mean red
  redsd <- .8 # population sd red
  yellowmean <- 3.1 # population mean yellow
  yellowsd <- .8 # population sd yellow
  n <- 20 #samplesize
  
  # container for samples
  samples <- reactiveValues(
    lastsamplered = as.numeric(),
    lastsampleyellow = as.numeric(),
    hist = as.numeric()
  )
  #Make a red and yellow population
  population <- data.frame(red = rnorm(n = N, mean = redmean, sd =redsd),
                           yellow = rnorm(n = N, mean = yellowmean,sd = yellowsd))
    #Smallsamplebuttonpress
    observeEvent(input$smallsamplebutton,{
               samples$lastsamplered <<- rnorm(n = n, 
                                                 mean = redmean,
                                                 sd = redsd
                                                 )
               samples$lastsampleyellow <<- rnorm(n = n,
                                                 mean =yellowmean,
                                                 sd = yellowsd
                                                    )
               meantemp <- mean(samples$lastsamplered) - mean(samples$lastsampleyellow)
               samples$hist <<- c(samples$hist,meantemp)
               
               # Limit sample size to 3 Mb
               if (object.size(samples) > 3e+06) {
                 samples$lastsamplered <<- as.numeric()
                 samples$lastsampleyellow <<- as.numeric()
                 samples$hist <<- as.numeric()
               }
               }
               )
   #Largesamplebuttonpress
   observeEvent(input$largesamplebutton,{
     largesamplered <- replicate(rnorm(n = n, 
                                       mean = redmean,
                                       sd = redsd
                                       ),n = 1000)
     largesampleyellow <- replicate(rnorm(n = n,
                                          mean =yellowmean,
                                          sd = yellowsd
                                          ),n = 1000)
      r <- apply(largesamplered,MARGIN = 2, mean) 
      y <-apply(largesampleyellow,MARGIN = 2, mean)
      
      samples$lastsamplered <<- largesamplered[ ,n]
      samples$lastsampleyellow <<- largesampleyellow[,n]
      
      samples$hist <<- c(samples$hist, r-y) 
      
      # Limit sample size to 3 Mb.
      if (object.size(samples) > 3e+06) {
        samples$lastsamplered <<- as.numeric()
        samples$lastsampleyellow <<- as.numeric()
        samples$hist <<- as.numeric()
      }
      }
   )
   #Resetbuttonpress
   observeEvent(input$resetbutton,{
     samples$lastsamplered <<- as.numeric()
     samples$lastsampleyellow <<- as.numeric()
     samples$hist <<- as.numeric()
   }
   )
   #Red population plot
   output$redpopplot <- renderPlot({
    ggplot(population,aes(x = red )) + 
      geom_dotplot(fill = brewercolors["Red"]) +
      geom_vline(size = .7,
                 aes(xintercept = redmean,
                     linetype = paste("Average =",redmean))) +
      coord_cartesian(xlim = c(0,6)) +
      scale_x_continuous("Weight", breaks = seq(0,6,by = .5)) + 
      scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
      ggtitle("Population of red candy weights") +
      theme_general() + 
      scale_linetype_manual("", values = "dashed") +
      theme(legend.justification = c(1,1),
            legend.position = c(.99,.99))
     })
   
   #Yellow population plot
   output$yellowpopplot <- renderPlot({
     ggplot(population,aes(x = yellow )) + 
       geom_dotplot(fill = brewercolors["Yellow"]) + 
       geom_vline(size = .7,
                  aes(xintercept = yellowmean,
                      linetype = paste("Average =",yellowmean))) +
       coord_cartesian(xlim = c(0,6)) +
       scale_x_continuous("Weight",breaks = seq(0,6,by = .5)) + 
       scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
       ggtitle("Population of yellow candy weights") +
       theme_general() + 
       scale_linetype_manual("", values = "dashed") +
       theme(legend.justification = c(1,1),
             legend.position = c(.99,.99))
   })
   #Red sample plot
   output$redsampplot <- renderPlot({
     validate(
       need(samples$lastsamplered != "", "")
     )
     df <- data.frame(red = samples$lastsamplered)
     ggplot(df, aes(x = red)) + 
       geom_dotplot(method = "histodot",
                    binwidth = .15,
                    fill = brewercolors["Red"],
                    dotsize = 1) + 
       geom_vline(size = .7,
                  aes(xintercept = mean(red),
                      linetype = paste("Average = ",
                                       round(mean(red), digits = 2)))) +
       coord_cartesian(xlim = c(0,6)) + 
       scale_x_continuous("Weight", breaks = seq(0,6,by = .5)) + 
       scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
       ggtitle("Last sample of red candy weights") +
       theme_general() + 
       scale_linetype_manual("", values = "dashed") +
       theme(legend.justification = c(1,1),
             legend.position = c(.98,.98))
   }) 
   #Yellow sample plot
   output$yellowsampplot <- renderPlot({
     validate(
       need(samples$lastsampleyellow != "", "")
     )
     df <- data.frame(yellow = samples$lastsampleyellow)
     ggplot(df, aes(x = yellow)) + 
       geom_dotplot(method = "histodot",
                    binwidth = .15,
                    fill = brewercolors["Yellow"],
                    dotsize = 1
                    ) + 
       geom_vline(size = .7,
                  aes(xintercept = mean(yellow),
                      linetype = paste("Average = ",
                                       round(mean(yellow), digits = 2)))) +
       coord_cartesian(xlim = c(0,6)) + 
       scale_x_continuous("Weight", breaks = seq(0,6,by = .5)) + 
       scale_y_continuous(name = NULL , labels = NULL, breaks = NULL) +
       ggtitle("Last sample of yellow candy weights") +
       theme_general() + 
       scale_linetype_manual("", values = "dashed") +
       theme(legend.justification = c(1,1),
             legend.position = c(.98,.98))
   }) 
   #Text showing most recent calculation
   output$calculationtext <- renderText({
     validate(
       need(samples$lastsamplered != "", "")
     )
     red <- round(mean(samples$lastsamplered), digits = 2)
     yellow <- round(mean(samples$lastsampleyellow), digits = 2)
     paste("<font color=\"#FF0000\">",
           red,
           "</font>",
           "-",
           "<font color=\"#FFD300\">",
           yellow,
           "</font>",
           "=",
           round(red - yellow,digits = 2)
           )
     })
   #Sampling distribution plot
   output$sampdistplot <- renderPlot({
     validate(
       need(samples$hist != "", "Please start by drawing a sample")
     )
     df <- data.frame(means = samples$hist)
     ggplot(df, aes(x = means)) + 
       geom_histogram(fill = "grey",
                      colour = "black",
                      alpha = .8,
                      binwidth = .1) +
       coord_cartesian(xlim = c(-1.05,1.05)) + 
       scale_x_continuous(name = "Mean differences", breaks = seq(-1,1,by = 0.1)) +
       ggtitle("Sampling distribution of mean differences") +
       theme_general()
     })
})
