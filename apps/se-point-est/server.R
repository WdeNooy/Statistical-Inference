library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

shinyServer(function(input, output) {
  #Load general plot theme and colors for color brewer
  source("../plottheme/styling.R")
  #VARIABLES
  N <- 10 #Sample size
  reps <- 100 #Repetitions of samples
  mean <- 2.8 # Population mean
  sd <- 1 #Population sd
  se <- sd/sqrt(N)
  
  #container for samples
  samples <- reactiveValues(
    lastsample = numeric(),
    hist = numeric())
  
  ##BUTTON ACTIONS
  #Take small sample 
  observeEvent(input$smallsamplebutton,{
    samples$lastsample <<- rnorm(N,mean = mean, sd = sd)
    samples$hist <<- c(samples$hist, mean(samples$lastsample))
    })
  
  #Take largesample
  observeEvent(input$largesamplebutton,{
    temp <- replicate(n = reps, rnorm(N,mean = mean, sd = sd))
    samples$hist <<- c(samples$hist,apply(temp, MARGIN = 2, mean))
    samples$lastsample <<- temp[ , reps]
  })
  
  #Reset
  observeEvent(input$resetbutton,{
    samples$hist <<- numeric()
    samples$lastsample <<- numeric()
    })
  
  #PLOT OF SINGLE SAMPLE
  output$sampleplot <- renderPlot({
    
    #Display message if there is no data yet
    validate(
      need(length(samples$lastsample) != 0, "Please draw a sample")
      )
    #data frame
    df <- data.frame(cweight = samples$lastsample)
  
    #PLOT
    ggplot(df,aes(x = cweight)) +
      #Dotplot
      geom_dotplot(method = "histodot",
                   binwidth = .2,
                   fill = sample(brewercolors,
                                 size = N,
                                 replace = TRUE)) +
      #Sample Mean line
      geom_vline(size = 0.7,
                 aes(xintercept = mean(cweight),
                     linetype = "Sample Mean")) + 
      #Population Mean line
      geom_vline(colour = "black",
                 size = 1,
                 aes(xintercept = mean,
                     linetype = "Population Mean")) + 
      #Text indicating N
      geom_text(aes(x = 5,
                y = 1,
                label = paste("N =",N,sep = ""))) +
      #Arrow
      geom_segment(aes(x = mean,
                       xend = mean(cweight, na.rm = TRUE),
                       y = .5,
                       yend = .5,
                       colour = "Deviation from mean"),
                       size = 1,
                       arrow = arrow(length = unit(.2,"cm"))) + 
      #Specifing legend for linetypes
      scale_linetype_manual(guide = guide_legend(title = ""),
                            values = c("Sample Mean" = "dashed",
                                       "Population Mean" = "solid")) +
      #Specifing legend for colour/arrow
      scale_colour_manual(guide = guide_legend(title = ""),
                          values = c("Deviation from mean" = unname(brewercolors["Red"]))) + 
      #Adjusting zoom
      coord_cartesian(xlim = c(0,6)) +
      #Setting breaks
      scale_x_continuous(breaks = c(seq(0,6,by = .5))) + 
      scale_y_continuous(breaks = NULL) + 
      #Title and axis labels
      ggtitle("Last sample") +
      ylab("") + 
      xlab("Candy weight") +
      #General theme
      theme_general() +
      #Adjusting legend
      theme(legend.position = "top",
            legend.margin = margin(.1, .1, .1, .1, unit = "cm"),
            legend.text = element_text(size = 9))
    })
  #PLOT OF SAMPLING DISTRIBUTION
  output$sampdistplot <- renderPlot({
    validate(
      need(length(samples$hist) != 0, "")
    )
    
    binwidth = .2 #histogram bindwidth
    
    #data frame
    df <- data.frame(mns = samples$hist)
    
    #PLOT
    ggplot(df,aes(x = mns)) + 
      #Normal distribution in background
      stat_function(data = data.frame(x = seq(0,6,by = .1)),
                    aes(x = x,
                        fill = "True distribution"),
                    color = "black",
                    geom = "area",
                    alpha = .3,
                    #function for scaling distribution to be visible in plot
                    fun = function(x, mean, sd, n, bw) {
                      dnorm(x = x, mean = mean, sd = sd) * n * bw
                    },
                    args = c(
                      mean = mean,
                      sd = sd/sqrt(N),
                      n = 50 + length(df$mns),
                      bw = binwidth
                    )) +
      #Histogram
      geom_histogram(binwidth = binwidth,
                     fill = brewercolors["Blue"],
                     color = "grey") +
      #Mean line
      geom_vline(aes(xintercept = mean,
                     linetype = "True mean"),
                    size = 1) +
      #Left standard error line
      geom_vline(aes(xintercept = mean - sd/sqrt(N),
                     linetype = "± 1 Standard error"),
                 size = .5
                 ) +
      #Left standard error line
      geom_vline(aes(xintercept = mean + sd/sqrt(N),
                     linetype = "± 1 Standard error"),
                 size = .5
      ) +
      #Arrow left
      geom_segment(aes(x = mean(df$mns, na.rm = TRUE),
                       xend = mean(df$mns, na.rm = TRUE) - sd(df$mns, na.rm = TRUE),
                       y = 0.5 + .01 * length(df$mns),
                       yend = 0.5 + .01 * length(df$mns),
                       colour = "± 1 Standard deviation (data)"),
                   size = 1,
                   arrow = arrow(length = unit(.2,"cm"))) +
      #Arrow right
      geom_segment(aes(x = mean(df$mns, na.rm = TRUE),
                       xend = mean(df$mns, na.rm = TRUE) + sd(df$mns, na.rm = TRUE),
                       y = 0.5 + .01 * length(df$mns),
                       yend = 0.5 + .01 * length(df$mns),
                       colour = "± 1 Standard deviation (data)"),
                   size = 1,
                   arrow = arrow(length = unit(.2,"cm"))) +
      #Mean (data)
      geom_point(aes(x = mean(df$mns, na.rm = TRUE),
                     y = 0.5 + .01 * length(df$mns),
                     colour = "± 1 Standard deviation (data)"),
                 size = 3) +
      #Zoom level
      coord_cartesian(xlim = c(1,5)) + 
      #Tickmarks
      scale_x_continuous(breaks = seq(0,6,by = .5)) + 
      #Legend definitions
      scale_fill_manual("",values = c("True distribution" = unname(brewercolors["Green"]))) +
      scale_color_manual("", values = c("± 1 Standard deviation (data)" = unname(brewercolors["Red"]))) +
      scale_linetype_manual("",values = c("± 1 Standard error" = "dashed",
                                          "True mean" = "solid")) +
      #General theme
      theme_general() + 
      #Setting title and axis labels
            ggtitle("Sampling distribution") +
      ylab("Count") + 
      xlab("Means of sample weights") +
      #Legend position adjustment
      guides(linetype = guide_legend(nrow = 2,reverse = TRUE)) + 
      theme(legend.position = "top",
            legend.margin = margin(0, 0, 0, 0, unit = "cm"),
            legend.direction = "horizontal",
            legend.text = element_text(size = 8))
  })
})
