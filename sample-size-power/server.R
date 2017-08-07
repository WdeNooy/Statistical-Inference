library(shiny)
library(pwr)
library(ggplot2)

shinyServer(function(input, output) {

  #source("../plottheme/styling.R", local = TRUE)
  



output$mainplot <- renderPlot({

  meanh0 = 0
  sd <- 3
  n <-  ceiling(pwr.t.test(d = input$efsizeslider,sig.level = 1 - (input$siglevslider/100), power = input$powerslider/100)$n)
  df <- n - 1 

  lefth0 <- meanh0 - qt((1-(1-input$siglevslider/100)/2),df = df)
  if(input$onetwoselect == "Two-sided"){
    righth0 <- meanh0 + qt((1-(1-input$siglevslider/100)/2),df = df)
  }
  else{
    righth0 <- meanh0 + qt(input$siglevslider/100,df = df)
  }
  
  d <- input$efsizeslider
  
  meanha <- meanh0 + d 
  
  print(n)
  #dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  
 p <-
   ggplot(data.frame(x = c(-5,8)), aes(x = x)) + 
    ##H0

    #H0 distribution function
    stat_function(fun = dt,
                  args = list(df = df, ncp = 0),
                  n = 1000) +
  ##Ha
  #beta-1/power
  stat_function(fun = dt,
                xlim = c(righth0,10),
                geom = "area",
                fill = brewercolors["Blue"],
                colour = "black",
                alpha = 0.5,
                args = list(df = df, ncp = d*sqrt(n)),
                n = 1000) +
    #Beta
    stat_function(fun = dt,
                  xlim = c(0,righth0),
                  geom = "area",
                  colour = "black",
                  fill = brewercolors["Yellow"],
                  alpha = 0.5,
                  args = list(df = df, ncp = d*sqrt(n)),
                  n = 1000) +
    #Ha distribution function
    stat_function(fun = dt,
                  args = list(df = df, ncp = d*sqrt(n)),
                  alpha = 0.5,
                  n = 1000)
  theme_general()
  
  if(input$onetwoselect == "Two-sided"){
    p + #Left alpha
      stat_function(fun = dt,
                    xlim = c(-5,lefth0),
                    geom = "area",
                    fill = brewercolors["Red"],
                    colour = "black",
                    alpha = 0.8,
                    args = list(df = df, ncp = 0),
                    n = 1000) +
      #Right alpha 
      stat_function(fun = dt,
                    xlim = c(righth0,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Red"],
                    args = list(df = df, ncp = 0),
                    n = 1000)
  }else{
    p +  #Right alpha 
      stat_function(fun = dt,
                    xlim = c(righth0,10),
                    geom = "area",
                    colour = "black",
                    fill = brewercolors["Red"],
                    args = list(df = df, ncp = 0),
                    n = 1000)
  }
})
  
  
})
