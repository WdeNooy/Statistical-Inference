library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)
  
  se <- 0.7  #sd of sampling distribution
  df <- 30 #df of t distribution
  sampsd <- 1.0 #sd of sample
  tc <- qt(0.05, df, lower.tail = FALSE) #criticial quantiles
  under <- -.10 #margin below sampling distribution
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
  #Reactive containers for changing values
  reactive <- 
    reactiveValues(
      sampmean = ifelse(exists("react$sample"), runif(1, 3, 8), 3.9) #initial saple mean = 3.9
    )
  react <- 
    reactiveValues(
      sample = data.frame(x = rnorm(df + 1, mean = isolate(reactive$sampmean), sd = sampsd), y = 0.45) #sample
    )
  
  #Reset
  observeEvent(input$resetButton, {
    reactive$sampmean <- runif(1, 3, 8) #sample mean
    react$sample <- data.frame(x = rnorm(df + 1, mean = reactive$sampmean, sd = sampsd), y = 0.45) #sample
  })
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    mean <- input$sampsizeslider #hypothesized population mean
    
    #Calculating the left threshold
    left <- mean - se * tc
    
    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area 0 curve: 5%
      stat_function(fun = dtshift,
                    xlim = c(1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #5% label leftt
      geom_text(label = "5%",
                aes(x = left - 0.4,
                    y =  dtshift(left, mean, se, df) + 0.03),
                hjust = 0,
                size = 5) +
      #Horizontal axis for sampling distribution
      geom_hline(aes(yintercept = 0)) +
      #Hypothesized population mean line
      geom_segment(aes(x = mean, xend = mean, 
                       y = under, yend = dtshift(mean, mean, se, df))) +
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
      #Scaling and double axis definitions
      scale_x_continuous(breaks = c(1, left, mean, 10), limits = c(1, 10),
                         labels = c("1", format(round(left, 2),nsmall = 2), 
                                    format(round(mean, 2),nsmall = 2), "10"),
                         sec.axis = sec_axis(~ .,
                           breaks = c(1, reactive$sampmean, 10),
                           labels = c(1, 
                                      paste0("Mean = ",round(reactive$sampmean, digits = 2)),
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
