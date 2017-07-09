library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)
  
  se <- 1.25  #sd of sampling distribution
  df <- 30 #df of t distribution
  sampsd <- 0.8 #sd of sample
  mean <- 5.5 #hypothesized population mean

  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
  #Reactive containers for changing values
  reactive <- 
    reactiveValues(
      sampmean = runif(1, 3, 8) #sample mean
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
    
    #Type of test
    if(input$slider[1] == 1 & input$slider[2] == 10) { #no test
      p_label <- ""
      p <- ""
      sig <- "not applicable"
    }
    else if(input$slider[1] == 1 & input$slider[1] != input$slider[2]) { #right-sided test
      p_label <- "Right-sided p"
      p <- paste0(format(round(pt((reactive$sampmean - mean)/se, df,
                                  lower.tail = FALSE), digits = 3), nsmall = 3))
      sig <- paste0(format(round(pt((input$slider[2] - mean)/se, df,
                                    lower.tail = FALSE), digits = 3), nsmall = 3))
    }
    else {if(input$slider[2] == 10 & input$slider[1] != input$slider[2]) { #left-sided test
      p_label <- "Left-sided p"
      p <- paste0(format(round(pt((reactive$sampmean - mean)/se, df,
                                  lower.tail = TRUE), digits = 3), nsmall = 3))
      sig <- paste0(format(round(pt((input$slider[1] - mean)/se, df,
                                    lower.tail = FALSE), digits = 3), nsmall = 3))
    }
    else {if(round(input$slider[1],3) == round(2*mean - input$slider[2],3) & input$slider[1] != input$slider[2]) { #two-sided
      p_label <- "Two-sided p"
      p <- paste0(format(2*round(pt((reactive$sampmean - mean)/se, df,
                                  lower.tail = (reactive$sampmean <= mean)), digits = 3), nsmall = 3))
      sig <- paste0(format(2*round(pt((input$slider[2] - mean)/se, df,
                                    lower.tail = FALSE), digits = 3), nsmall = 3))
    }
    else { #unbalanced tails
      p_label <- ""
      p <- ""
      sig <- "not applicable"
    }}}
    
    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area 0 curve
      stat_function(fun = dtshift,
                    xlim = c(1,input$slider[1]),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Right area 0 curve"2.5%
      stat_function(fun = dtshift,
                    xlim = c(input$slider[2],10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #p value label left
      geom_text(label = ifelse(input$slider[1] == 1, "0.000", 
                               paste0(format(round(pt((input$slider[1] - mean)/se, df, lower.tail = TRUE), 
                                                   3), nsmall = 3))),
                aes(x = input$slider[1] ,
                    y =  dtshift(input$slider[1], mean, se, df) + 0.03),
                colour = brewercolors["Blue"],
                hjust = ifelse(input$slider[1] < 2 | input$slider[1] > 5.5, 0, 1),
                size = 5) +
      #p value label right
      geom_text(label = ifelse(input$slider[2] == 10, "0.000", 
                               paste0(format(round(pt((input$slider[2] - mean)/se, df, lower.tail = FALSE), 
                                                   3), nsmall = 3))),
                aes(x = input$slider[2] ,
                    y =  dtshift(input$slider[2], mean, se, df) + 0.03),
                colour = brewercolors["Blue"],
                hjust = ifelse(input$slider[2] > 9 | input$slider[2] < 5.5, 1, 0),
                size = 5) +
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
      #Sample average p value label
      geom_text(label = p_label,
                aes(x = reactive$sampmean + ifelse(reactive$sampmean >= mean, 0.1, -0.1),
                    y =  0.4),
                colour = brewercolors["Red"],
                hjust = ifelse(reactive$sampmean >= mean, 0, 1),
                size = 5) +
      #Sample average p value
      geom_text(label = p,
                aes(x = reactive$sampmean + ifelse(reactive$sampmean >= mean, 0.1, -0.1),
                    y =  0.35),
                colour = brewercolors["Red"],
                hjust = ifelse(reactive$sampmean >= mean, 0, 1),
                size = 5) +
      #Sample average significance label
      geom_rect(aes(xmin = 4.1, xmax = 6.9, ymin = 0.02, ymax = 0.12), fill = "White") +
      geom_text(label = "Significance level",
                aes(x = 5.5 ,
                    y =  0.1),
                colour = brewercolors["Blue"],
                hjust = 0.5,
                size = 5) +
      #Sample average significance label
      geom_text(label = sig,
                aes(x = 5.5 ,
                    y =  0.05),
                colour = brewercolors["Blue"],
                hjust = 0.5,
                size = 5) +
      #Scaling and double axis definitions
      scale_x_continuous(breaks = c(1, input$slider[1], mean, input$slider[2], 10),
                         limits = c(1, 10),
                         labels = c(1, format(round(input$slider[1],2), nsmall=2),format(round(mean,2), nsmall=2),format(round(input$slider[2],2), nsmall=2), 10),
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
