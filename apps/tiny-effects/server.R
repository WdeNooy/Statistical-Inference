library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #Source styling file for plots
  source("../plottheme/styling.R", local = TRUE)

  mean <- 2.8 #Hypothesized population mean
  sdpop <- 0.6 #Population SD 

  ##RENDER MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #Hardcoded labels for plot
    strengthlab <- c("Strong\n2.32",
                     "Moderate\n2.5",
                     "Weak\n2.68",
                     "H0\n2.8",
                     "Weak\n2.92",
                     "Moderate\n3.1",
                     "Strong\n3.28")
    #Hardcoded tickmarks for plot
    ticks <- c(2.32,2.5,2.68,2.8,2.92,3.1,3.28) 
    
    SE <- sdpop/sqrt(input$ssizeslider) #SE
    
    error <- qnorm(1 - .025) * SE # Distance from mean
    left <- mean - error #Left confidence interval border
    right <- mean + error #Right confidence interval border
    
    sign <- paste0(ifelse(right <= input$savslider, 
                   "Statistically\nsignificant\ntest result for a\n", 
                   "Statistically\nnon-significant\ntest result for a\n"),
                   ifelse(
                     input$savslider == 2.8,
                     "really no effect at all",
                     ifelse(input$savslider < 2.83,
                            "negligible effect size",
                            ifelse(
                              input$savslider < 2.87,
                              "very weak\neffect size",
                              ifelse(input$savslider < 3.01,
                                     "weak effect size",
                                     ifelse(input$savslider < 3.2,
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
      input$effectslider == 2.8, NA, round(
        power.t.test(n = input$ssizeslider,
                     delta = (input$effectslider - mean),
                     sd = sdpop,
                     sig.level = 0.05,
                     type = "one.sample",
                     alternative = "two.sided")$power,
        2)
    )
    
    
     #PLOT
     p <- ggplot(data.frame(x = c(0,6)), aes(x = x)) 
     if (input$showpower) {
       p <- p +
        #Power: left area under curve
         stat_function(fun = dnorm,xlim = c(-10,left),
                       geom = "area",
                       fill = brewercolors["Green"],
                       colour = "grey",
                       alpha = 1,
                       args = list(mean = input$effectslider, sd = SE),
                       n = 1000) + 
        #Power: right area under curve
       stat_function(fun = dnorm,
                     xlim = c(right,10),
                     geom = "area",
                     colour = "grey",
                     alpha = 1,
                     fill = brewercolors["Green"],
                     args = list(mean = input$effectslider, sd = SE),
                     n = 1000) +
       #Normal function line for true sampling distribution 
       stat_function(fun = dnorm,
                     args = list(mean = input$effectslider, sd = SE),
                     n = 1000,
                     color = "grey") +
         #Population mean vline
         geom_vline(aes(xintercept = input$effectslider,
                        linetype = "True population mean",
                        colour = "True population mean"),
                    size = 0.8) +
         #Power
         geom_text(label = paste0("Power ", power),
                   aes(x = 3.45, 
                       y = 1.1 * dnorm(mean, mean, SE)),
                   color = brewercolors["Green"],
                   size = 5,
                   vjust = 1, 
                   hjust = 1
         )
       }
      # show plot
     p +
      #Left area under curve
      stat_function(fun = dnorm,xlim = c(-10,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = "black",
                    alpha = 0.5,
                    args = list(mean = mean, sd = SE),
                    n = 1000) + 
      #Right area under curve
      stat_function(fun = dnorm,
                    xlim = c(right,10),
                    geom = "area",
                    colour = "black",
                    alpha = 0.5,
                    fill = brewercolors["Blue"],
                    args = list(mean = mean, sd = SE),
                    n = 1000) +
      #Normal function line 
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = SE),
                    n = 1000) +
      #2,5% label right
      geom_text(label = "2.5%",
                aes(x = right * 1.01 ,
                    y =  dnorm(right, mean, SE)),
                hjust = 0,
                size = 5) +
      #2.5%label left
      geom_text(label = "2.5%",
                aes(x = left * 0.99 ,
                    y =  dnorm(left, mean, SE)),
                hjust = 1,
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
                aes(x = 2.15, 
                    y = 1.1 * dnorm(mean, mean, SE)),
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
      scale_x_continuous(name = "Average candy weight", breaks = ticks, labels = strengthlab) + 
      scale_y_continuous(name = "Probability density", breaks = NULL) +
      #Defining x axis zoom
      coord_cartesian(xlim = c(2.15, 3.45)) +
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
