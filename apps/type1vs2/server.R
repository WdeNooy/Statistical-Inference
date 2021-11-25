library(shiny)
library(pwr)
library(ggplot2)

shinyServer(function(input, output) {

  source("../plottheme/styling.R", local = TRUE)
  
  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) dt(x = (x - mean)/sd, df = df)
  under <- -.05 #margin below sampling distribution
  
output$mainplot <- renderPlot({
  
  # set limits of x axis
  xmin <- 2.7
  xmax <- 3.0
  
  # set n and calculate df from given n
  n <- 220 #sample size
  df <- n - 1 
  meanh0 = 2.8 # Mean of h0
  sd <- 0.5 # SD of population
  se <- sd / sqrt(n) # SE of sampling distribution
  meanha <- 2.9 # True mean
  meansample <- 2.84 # Sample mean  
  # power (as percentage)
  power <- round(pwr.t.test(d = (meanha - meanh0)/sd,
                             sig.level = 0.05,
                             type = "one.sample",
                             alternative = "two.sided",
                             n = n)$power*100) 
  
  
  # calculate 95 % confidence interval
  lefth0 <- meanh0 - se*qt(0.975,df = df)
  righth0 <- meanh0 + se*qt(0.975,df = df)

  p <- ggplot(data.frame(x = c(xmin,xmax)), aes(x = x)) + 
    ##H0
    #Left alpha
    stat_function(fun = dtshift,
                  xlim = c(xmin,lefth0),
                  geom = "area",
                  fill = brewercolors["Blue"],
                  colour = "black",
                  alpha = ifelse(
                    input$steps %in% c("step1", "step2"),
                    0.5, #show
                    0 #don't show
                  ),
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000) +
    #Right alpha 
    stat_function(fun = dtshift,
                  xlim = c(righth0,xmax),
                  geom = "area",
                  colour = "black",
                  fill = brewercolors["Blue"],
                  alpha = ifelse(
                    input$steps %in% c("step1", "step2"),
                    0.5, #show
                    0 #don't show
                  ),
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000) +
    #Left alpha 2.5% label
    geom_text(
      aes(x = lefth0, y = 0.015,
          label = "2.5%",
          hjust = 1
          ),
      size = 3
    ) +
    #Right alpha 2.5% label
    geom_text(
      aes(x = righth0, y = 0.015,
          label = "2.5%",
          hjust = 0
      ),
      size = 3
    ) +
    #H0 distribution function
    stat_function(fun = dtshift,
                  xlim = c(xmin, xmax),
                  args = list(mean = meanh0, sd = se, df = df),
                  n = 1000) +
    #Hypothesized population (H0) mean line
    geom_segment(aes(x = meanh0, xend = meanh0, 
                     y = 0, yend = Inf)) +
    ##Sample
    #vertical line
    geom_vline(xintercept = meansample,
               colour = brewercolors["Red"]
    ) +
    geom_text(
      aes(x = meansample, y = 0.5 * under,
          label = paste0("Sample average: ", round(meansample, digits = 2)),
          vjust = .5,
          hjust = 1.05),
      colour = brewercolors["Red"]
    ) +
    theme_general() +
    scale_x_continuous(name = "Average candy weight in a sample (grams)",
                       limits = c(xmin, xmax),
                       breaks = c(lefth0, righth0), 
                       labels = c(round(lefth0, 2), round(righth0, 2)),
    sec.axis = sec_axis(~ .,
                       name = "Average candy weight in the population (grams)", 
                       breaks = if(input$steps == "step1") {
                         c(meanh0)
                       } else {
                         c(meanh0, meanha)
                       }, 
                       labels = if(input$steps == "step1") {
                         c(paste0(round(meanh0, 1), "\nH0"))
                       } else {
                         c(paste0(round(meanh0, 1), "\nH0"), 
                           paste0(round(meanha, 1), "\nH1"))
                       })
    ) +
    scale_y_continuous(breaks = NULL, 
                     limits = c(under, 0.45),
                     name = "Probability density",
                     expand = c(0, 0)) 
    
  #compose plot parts depending on step
  #only step 4 shows power
  if (input$steps == "step4") {
  p <- p +
    #power
    stat_function(fun = dtshift,
                  xlim = c(righth0,xmax),
                  geom = "area",
                  fill = brewercolors["Green"],
                  colour = "black",
                  alpha = 0.5,
                  args = list(mean = meanha, sd = se, df = df),
                  n = 1000) +
    #power result
    geom_text(
      aes(x = meanha + 0.01, y = 0.1, 
          label = paste0(round(power), "%"),
          hjust = 0
      ),
      colour = "black",
      size = 5
    )}
  #Steps 3 and 4 show beta
    if (input$steps %in% c("step3","step4")) {
      p <- p +
        #Beta
        stat_function(fun = dtshift,
                      xlim = c(xmin,righth0),
                      geom = "area",
                      colour = "black",
                      fill = brewercolors["Yellow"],
                      alpha = 0.5,
                      args = list(mean = meanha, sd = se, df = df),
                      n = 1000) +
        #power boundary
        geom_segment(aes(x = righth0, xend = righth0, 
                         y = under, yend = dtshift(righth0, meanha, se, df)))
    }
  #Steps 2, 3 and 4 show the alternative sampling distribution
  if (input$steps != "step1") {
    p <- p +
      ##Ha
      #Ha distribution function
      stat_function(fun = dtshift,
                    xlim = c(xmin, xmax),
                    args = list(mean = meanha, sd = se, df = df),
                    n = 1000) +
      #Hypothesized population (H1) mean line
      geom_segment(aes(x = meanha, xend = meanha, 
                       y = 0, yend = Inf))
  }
  #All steps show the (non)rejection regions (on top)
  p +
      #non-rejection area
      geom_segment(aes(x = lefth0, xend = righth0, y = 0, yend = 0),
                   colour = "black"
      ) +
      #Left rejection region
      geom_segment(aes(x = xmin, xend = lefth0, y = 0, yend = 0),
                   colour = brewercolors["Blue"], 
                   alpha = 0.5,
                   size = 1.5
      ) +
      #Right rejection region
      geom_segment(aes(x = righth0, xend = xmax, y = 0, yend = 0),
                   colour = brewercolors["Blue"], 
                   alpha = 0.5,
                   size = 1.5
      ) +       
      #Left rejection boundary
      geom_segment(aes(x = lefth0, xend = lefth0, 
                   y = under, yend = dtshift(lefth0, meanh0, se, df))) +       
      #Right rejection boundary
      geom_segment(aes(x = righth0, xend = righth0, 
                     y = under, yend = dtshift(righth0, meanh0, se, df)))
  })

})
