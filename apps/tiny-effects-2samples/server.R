library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Source styling file for plots
  source("../plottheme/styling.R", local = TRUE)

  mean <- 0 #Hypothesized population mean difference
  sd <- 0.6 #Population SD 

  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) 0.8*dt(x = (x - mean)/sd, df = df)
  
  ##RENDER MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #Hardcoded labels for plot
    strengthlab <- c("Strong\n-0.48",
                     "Moderate\n-0.3",
                     "Weak\n-0.12",
                     "H0\n0",
                     "Weak\n0.12",
                     "Moderate\n0.3",
                     "Strong\n0.48")
    #Hardcoded tickmarks for plot
    ticks <- c(-0.48,-0.3,-0.12,0,0.12,0.3,0.48) 
    
    SE <- sqrt( 2 * sd^2/input$ssizeslider) #SE assuming equal population variances
    
    error <- qt(p = 1 - .025, df = input$ssizeslider - 2) * SE # Distance from mean
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
    
    sign <- paste0(ifelse(right <= input$savslider, 
                   "Statistically\nsignificant\ntest result for a\n", 
                   "Statistically\nnon-significant\ntest result for a\n"),
                   ifelse(
                     input$savslider == 0,
                     "really no effect at all",
                     ifelse(input$savslider < 0.02,
                            "negligible effect size",
                            ifelse(
                              input$savslider < 0.06,
                              "very weak\neffect size",
                              ifelse(input$savslider < 0.23,
                                     "weak effect size",
                                     ifelse(input$savslider < 0.41,
                                            "moderately strong\neffect size",
                                            "strong effect size"
                                            )
                                    )
                                  )
                          )
                     ),
                   "\nin the sample."
                   )
    #calculate power and store
    power <- ifelse(
      input$effectslider == 0, NA, round(
        power.t.test(n = input$ssizeslider / 2, #n is size of group
                     delta = (input$effectslider - mean),
                     sd = sd,
                     sig.level = 0.05,
                     type = "two.sample",
                     alternative = "two.sided")$power,
        2)
    )
    
    
     #PLOT
     p <- ggplot(data.frame(x = c(-1, 1)), aes(x = x)) 
     if (input$showpower) {
       p <- p +
        #Power: left area under curve
         stat_function(fun = dtshift,
                       xlim = c(-1,left),
                       geom = "area",
                       fill = brewercolors["Green"],
                       colour = "grey",
                       alpha = 1,
                       args = list(mean = input$effectslider, sd = SE, df = input$ssizeslider - 2),
                       n = 1000) + 
        #Power: right area under curve
       stat_function(fun = dtshift,
                     xlim = c(right,1),
                     geom = "area",
                     colour = "grey",
                     alpha = 1,
                     fill = brewercolors["Green"],
                     args = list(mean = input$effectslider, sd = SE, df = input$ssizeslider - 2),
                     n = 1000) +
       #t function line for true sampling distribution 
       stat_function(fun = dtshift,
                     args = list(mean = input$effectslider, sd = SE, df = input$ssizeslider - 2),
                     n = 1000,
                     color = "grey") +
         #Population mean vline
         geom_vline(aes(xintercept = input$effectslider,
                        linetype = "True population mean",
                        colour = "True population mean"),
                    size = 0.8) +
         #Power
         geom_text(label = paste0("Power ", power),
                   aes(x = 0.8, 
                       y = 1.1 * dt(input$effectslider, df = input$ssizeslider - 2)),
                   color = brewercolors["Green"],
                   size = 5,
                   vjust = 1, 
                   hjust = 1
         )
       }
      # show plot
     p +
      #Left area under curve
      stat_function(fun = dtshift,
                    xlim = c(-1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    alpha = 0.5,
                    args = list(mean = mean, sd = SE, df = input$ssizeslider - 2),
                    n = 1000) + 
      #Right area under curve
      stat_function(fun = dtshift,
                    xlim = c(right,1),
                    geom = "area",
                    colour = "black",
                    alpha = 0.5,
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = SE, df = input$ssizeslider - 2),
                    n = 1000) +
      #Normal function line 
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = SE, df = input$ssizeslider - 2),
                    n = 1000) +
      #2,5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.04 ,
                    y =  dtshift(right, mean = mean, sd = SE, df = input$ssizeslider - 2)),
                hjust = 0,
                vjust = 0,
                size = 5) +
      #2.5%label left
      geom_text(label = "2.5%",
                aes(x = left * 1.04 ,
                    y =  dtshift(left, mean = mean, sd = SE, df = input$ssizeslider - 2)),
                hjust = 1,
                vjust = 0,
                size = 5) +
      #Sampling distribution mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "Hypothesized population mean",
                     color = "Hypothesized population mean")) +
      #Sample mean vline
      geom_vline(aes(xintercept = input$savslider,
                      linetype = "Sample mean",
                 colour = "Sample mean"),
                 size = 0.8) +
      #Test result label
      geom_text(label = sign,
                aes(x = -0.8, 
                    y = 1.1 * dt(mean, df = input$ssizeslider - 2)),
                vjust = 1,
                hjust = 0,
                color = brewercolors["Blue"]
                ) +
      #Definition of sample mean type and legend name
      scale_linetype_manual(name = "",
                            values = c("Hypothesized population mean" = "dashed", 
                                       "Sample mean" = "solid", 
                                       "True population mean" = "solid")) + 
      scale_colour_manual(name = "", 
                             values = c("Hypothesized population mean" = "black", 
                                        "Sample mean" = brewercolors[["Blue"]], 
                                        "True population mean" = "grey")) + 
      #X axis breaks definition
      scale_x_continuous(name = "Average difference in understanding between the two groups", breaks = ticks, labels = strengthlab) + 
      scale_y_continuous(name = "Probability density", breaks = NULL) +
      #Defining x axis zoom
      coord_cartesian(xlim = c(-0.8, 0.8)) +
      #Title and labels for axes
      ggtitle("Sampling distribution") + 
      #Theme specification
      theme_general() + 
      theme(axis.text.x = element_text(size = 11),
            legend.text = element_text(size = 11.5),
            legend.position = "top",
            legend.direction = "horizontal")
  })
})
