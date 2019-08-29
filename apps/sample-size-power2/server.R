library(shiny)
library(pwr)
library(ggplot2)

shinyServer(function(input, output) {

  source("../plottheme/styling.R", local = TRUE)
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)

output$mainplot <- renderPlot({
  
  # Calculate power (as percentage) for given n
  n <- input$samsizeslider
  power <-  round(pwr.t.test(d = input$efsizeslider,
                           sig.level = (input$siglevslider/100),
                           type = "one.sample",
                           alternative = ifelse(input$onetwoselect == "Two-sided", "two.sided", "greater"),
                           n = n)$power*100) 
  # calculate df from given n
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
  meanha <- meanh0 + sd * input$efsizeslider #meanh0 plus unstandardized effect size
  
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
    theme_general() +
    scale_x_continuous(name = "Population means", limits = c(-5, 10), breaks = c(0, meanha), labels = c("H0", "H1")) +
    scale_y_continuous(name = "Probability density", breaks = NULL)
# Switch plot output dependent on one or two-sided selection  
if(input$onetwoselect == "Two-sided"){
  p + #Left alpha
    stat_function(fun = dtshift,
                  xlim = c(-5,lefth0),
                  geom = "area",
                  alpha = 0, #totally transparent, no fill colour
                  colour = "black",
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000) +
    #Left alpha 2.5% label
    geom_text(
      aes(x = lefth0, y = 0.01,
          label = paste0(format(round(input$siglevslider/2, digits = 1), nsmall = 1),"%"),
          hjust = 1
      ),
      size = 3,
      colour = "grey40"
    ) +
    #Right alpha 2.5% label
    stat_function(fun = dtshift,
                  xlim = c(righth0,10),
                  geom = "area",
                  colour = "black",
                  alpha = 0, #totally transparent, no fill colour
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000) +
    #Right alpha 
    geom_text(
      aes(x = righth0, y = 0.01,
          label = paste0(format(round(input$siglevslider/2, digits = 1), nsmall = 1),"%"),
          hjust = 0
      ),
      size = 3,
      colour = "grey40"
    )
 }else{
    p +  #Right alpha 
      stat_function(fun = dtshift,
                    xlim = c(righth0,10),
                    geom = "area",
                    colour = "black",
                    alpha = 0, #totally transparent, no fill colour
                    args = list(mean = meanh0, sd = se, df = df),
                    n = 1000) +
     #Right alpha 
     geom_text(
       aes(x = righth0, y = 0.01,
           label = paste0(format(round(input$siglevslider, digits = 1), nsmall = 1),"%"),
           hjust = 0
       ),
       size = 3,
       colour = "grey40"
     )
 }
    
})
output$ssizeuiout <- renderText({
  # Calculate necessary n for given power
  power <-  round(pwr.t.test(d = input$efsizeslider,
                             sig.level = (input$siglevslider/100),
                             type = "one.sample",
                             alternative = ifelse(input$onetwoselect == "Two-sided", "two.sided", "greater"),
                             n = input$samsizeslider)$power*100) 
  paste(paste0("Power: ", power, "%"))
  
  }) 
  
})
