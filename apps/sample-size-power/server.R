library(shiny)
library(pwr)
library(ggplot2)

shinyServer(function(input, output) {

  source("../plottheme/styling.R", local = TRUE)
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)

output$mainplot <- renderPlot({
  
  # Calculate necessary n for given power
  n <-  ceiling(pwr.t.test(d = input$efsizeslider,
                           sig.level = (input$siglevslider/100),
                           alternative = ifelse(input$onetwoselect == "Two-sided", "two.sided", "greater"),
                           power = input$powerslider/100)$n) 
  # calculate df for n with given power
  df <- n - 1 
  meanh0 = 0 # Mean of h0
  sd <- 6 # SD of population
  se <- sd / sqrt(n) # SE of sampling distribution

  # calculate 95 % confidence interval on left side
  lefth0 <- meanh0 - se*qt((1-(input$siglevslider/100)/2),df = df)
  # if two sided, calculate right limit dependent on one or two sided selection:
  if(input$onetwoselect == "Two-sided"){
    righth0 <- meanh0 + se*qt((1-(input$siglevslider/100)/2),df = df)
  }
  else{
    righth0 <- meanh0 + se*qt(1-input$siglevslider/100,df = df)
  }
  
  # True mean
  meanha <- righth0 + se*qt(input$powerslider/100, df = df) #Mean h0?
  
 p <-
   ggplot(data.frame(x = c(-5,8)), aes(x = x)) + 
  ##H0
  #H0 distribution function
   stat_function(fun = dtshift,
                 args = list(mean = meanh0, sd = se, df = df),
                 n = 1000) +
  ##Ha
  #beta-1/power
  stat_function(fun = dtshift,
                xlim = c(righth0,10),
                geom = "area",
                fill = brewercolors["Blue"],
                colour = "black",
                alpha = 0.5,
                args = list(mean = meanha, sd = se, df = df),
                n = 1000) +
    #Beta
    stat_function(fun = dtshift,
                  xlim = c(-5,righth0),
                  geom = "area",
                  colour = "black",
                  fill = brewercolors["Yellow"],
                  alpha = 0.5,
                  args = list(mean = meanha, sd = se, df = df),
                  n = 1000) +
    #Ha distribution function
    stat_function(fun = dtshift,
                  args = list(mean = meanha, sd = se, df = df),
                  alpha = 0.5,
                  n = 1000) +
    #Label with sample size
   # geom_label(x = 2.5, y = 0.35, label = paste0("Required sample size: ", n), size = 5) +
    theme_general() +
    scale_x_continuous(name = "Population means", limits = c(-5, 10), breaks = c(0, meanha), labels = c("H0", "H1")) +
    scale_y_continuous(name = "Probability density", breaks = NULL)
# Switch plot output dependent on one or two-sided selection  
if(input$onetwoselect == "Two-sided"){
  p + #Left alpha
    stat_function(fun = dtshift,
                  xlim = c(-5,lefth0),
                  geom = "area",
                  fill = brewercolors["Red"],
                  colour = "black",
                  alpha = 0.8,
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000) +
    #Right alpha 
    stat_function(fun = dtshift,
                  xlim = c(righth0,10),
                  geom = "area",
                  colour = "black",
                  fill = brewercolors["Red"],
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000)
 }else{
    p +  #Right alpha 
      stat_function(fun = dtshift,
                    xlim = c(righth0,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Red"],
                    args = list(mean = meanh0, sd = se, df = df),
                    n = 1000)
 }
    
})
output$ssizeuiout <- renderText({
  # Calculate necessary n for given power
  n <-  ceiling(pwr.t.test(d = input$efsizeslider,
                           sig.level = (input$siglevslider/100),
                           alternative = ifelse(input$onetwoselect == "Two-sided", "two.sided", "greater"),
                           power = input$powerslider/100)$n) 
  paste(paste0("Required sample size: ", n))
  
  }) 
  
})
