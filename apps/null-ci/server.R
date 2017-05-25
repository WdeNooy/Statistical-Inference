library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)
  
  se <- 0.7  #sd of sampling distribution
  df <- 30 #df of t distribution
  under <- -.15 #margin below sampling distribution
  
  sampsd <- 1.5
  sampmean <- runif(1, min = 3, max = 8) #sample mean
  sample <- data.frame(x = rnorm(df + 1, mean = sampmean, sd = sampsd), y = 0.45) #sample
  sample <- sample[sample$x >= 1 & sample$x <= 10,] #remove scores outside range
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    mean <- input$sampsizeslider #hypothesized population mean
    tc <- qt(0.025, df, lower.tail = FALSE) #criticial quantiles
    
    #Calculating the right and left threshold
    right <- mean + se * tc
    left <- mean - se * tc

    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area under curve
      stat_function(fun = dtshift,
                    xlim = c(1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Right area under curve
      stat_function(fun = dtshift,
                    xlim = c(right,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #2,5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.02 ,
                    y =  dtshift(right, mean, se, df) + 0.01),
                hjust = 0,
                size = 5) +
      #2.5% label left
      geom_text(label = "2.5%",
                aes(x = left * 0.98 ,
                    y =  dtshift(left, mean, se, df) + 0.01),
                hjust = 1,
                size = 5) +
      #Horizontal axis for sampling distribution
      geom_hline(aes(yintercept = 0)) +
      #Hypothesized population mean line
      geom_segment(aes(x = mean, xend = mean, 
                       y = under, yend = dtshift(mean, mean, se, df))) +
      #sample scores
      geom_point(data = sample, aes(x = x, y = y), 
                 colour = brewercolors["Red"]) +
      #Sample average vline
      geom_segment(aes(x = sampmean, xend = sampmean, 
                       y = 0, yend = 0.5), 
                   colour = brewercolors["Red"]) +
      #Scaling and double axis definitions
      scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 10),
                         sec.axis = sec_axis(~ .,
                           breaks = c(1, sampmean, 10),
                           labels = c(1, 
                                      paste0("Mean = ",round(sampmean, digits = 2)),
                                      10),
                           name = "Sample media literacy scores"),
                         expand = c(.02, .02)) +
      scale_y_continuous(breaks = NULL, 
                         limits = c(under, 0.5),
                         expand = c(0, 0)) + 
      #Axis labels and theme                                       
      xlab("Hypothesized population mean media literacy score") + 
      ylab("") + 
      theme_general() +
      theme(panel.border = element_rect(colour = NA), 
            plot.margin = margin(0,0,10,0))
  })
})
