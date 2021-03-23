library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)
  
  se <- 1.25  #sd of sampling distribution
  df <- 30 #df of t distribution
  sampsd <- 0.8 #sd of sample
  mean <- 5.5 #hypothesized population mean
  tc <- qt(0.025, df, lower.tail = FALSE) #criticial quantiles 2.5%
  tc5 <- qt(0.05, df, lower.tail = FALSE) #criticial quantiles 5%
  
  #Calculating the right and left threshold
  right <- mean + se * tc #2.5%
  left <- mean - se * tc #2.5%
  right5 <- mean + se * tc5 #5%
  left5 <- mean - se * tc5 #5%
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
  #Function for stratified randomization of sample mean.
  newsample <- function() {
    stratum <- round(runif(1, 0.51, 5.49))
    ifelse(stratum == 1, runif(1, 2, left),
      ifelse(stratum == 2, runif(1, left, left5),
        ifelse(stratum == 3, runif(1, left5, right5),
          ifelse(stratum == 4, runif(1, right5, right),
                 runif(1, right, 9)
          )            
        )            
      )
    )
  }
  
  #Reactive containers for changing values
  reactive <- 
    reactiveValues(
      sampmean = newsample() #sample mean
    )
  react <- 
    reactiveValues(
      sample = data.frame(x = rnorm(df + 1, mean = isolate(reactive$sampmean), sd = sampsd), y = 0.45) #sample
    )
  
  #Reset
  observeEvent(input$resetButton, {
    reactive$sampmean <- newsample() #sample mean
    react$sample <- data.frame(x = rnorm(df + 1, mean = reactive$sampmean, sd = sampsd), y = 0.45) #sample
  })
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area 0 curve: 2.5%
      stat_function(fun = dtshift,
                    xlim = c(1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Left area 0 curve: 5%
      stat_function(fun = dtshift,
                    xlim = c(1,left5),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    alpha = 0.3,
                    colour = "black",
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Right area 0 curve"2.5%
      stat_function(fun = dtshift,
                    xlim = c(right,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Right area 0 curve"5%
      stat_function(fun = dtshift,
                    xlim = c(right5,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    alpha = 0.3,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #2.5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.02 ,
                    y =  dtshift(right, mean, se, df) + 0.01),
                hjust = 0,
                size = 5) +
      #5% label right
      geom_text(label = "5%",
                aes(x = right5 * 1.02 ,
                    y =  dtshift(right5, mean, se, df) + 0.01),
                colour = brewercolors["Blue"],
                hjust = 0,
                size = 5) +
      #2.5% label left
      geom_text(label = "2.5%",
                aes(x = left * 0.98 ,
                    y =  dtshift(left, mean, se, df) + 0.01),
                hjust = 1,
                size = 5) +
      #5% label left
      geom_text(label = "5%",
                aes(x = left5 * 0.98 ,
                    y =  dtshift(left5, mean, se, df) + 0.01),
                colour = brewercolors["Blue"],
                hjust = 1,
                size = 5) +
      #Horizontal axis for sampling distribution
      geom_hline(aes(yintercept = 0)) +
      #Hypothesized population mean line
      geom_segment(aes(x = mean, xend = mean, 
                       y = 0, yend = dtshift(mean, mean, se, df))) +
      #sample scores
      geom_point(data = react$sample[react$sample$x >= 1 & react$sample$x <= 10,], aes(x = x, y = y), 
                 colour = brewercolors["Red"]) +
      #Sample average vline
      geom_segment(aes(x = reactive$sampmean, xend = reactive$sampmean, 
                       y = 0, yend = 0.5), 
                   colour = brewercolors["Red"]) +
      #Sample average p value (left)
      geom_text(label = paste0(format(round(pt((reactive$sampmean - mean)/se, df, 
                                 lower.tail = TRUE), 
                              digits = 3), nsmall = 3)),
                aes(x = reactive$sampmean ,
                    y =  0.2),
                colour = brewercolors["Red"],
                hjust = 1.1,
                size = 5) +
      #Sample average p value (right)
      geom_text(label = paste0(format(round(pt((reactive$sampmean - mean)/se, df, 
                                               lower.tail = FALSE), 
                                            digits = 3), nsmall = 3)),
                aes(x = reactive$sampmean ,
                    y =  0.2),
                colour = brewercolors["Red"],
                hjust = -0.1,
                size = 5) +
      #Scaling and double axis definitions
      scale_x_continuous(breaks = c(1, left, left5, mean, right5, right, 10),
                         limits = c(1, 10),
                         labels = c(1, format(round(left,1), nsmall=1), format(round(left5,1), nsmall=1), format(round(mean,1), nsmall=1), format(round(right5,1), nsmall=1), format(round(right,1), nsmall=1), 10),
                         sec.axis = sec_axis(~ .,
                           breaks = c(1, reactive$sampmean, 10),
                           labels = c(1, 
                                      paste0("Mean = ",round(reactive$sampmean, digits = 2)),
                                      10),
                           name = "Sample media literacy scores"),
                         expand = c(.02, .02)) +
      scale_y_continuous(breaks = NULL, 
                         limits = c(0, 0.5),
                         expand = c(0, 0)) + 
      #Axis labels and theme                                       
      xlab("Sample mean media literacy score") + 
      ylab("") + 
      theme_general() +
      theme(panel.border = element_rect(colour = NA), 
            plot.margin = margin(0,0,10,0))
  })
})
