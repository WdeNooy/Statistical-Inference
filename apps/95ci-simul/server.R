library(shiny)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  #Load styling files
  source("../plottheme/styling.R",local = TRUE)
  
  
  alpha <- 0.95 #confidence interval size
  mean <- 2.8
  sd <- 0.5
  N= 100 #Number of samples in total
  n = 50 #Sample size
  
  #Counter for action button etc
  counter <- reactiveVal(value = 1)

  #Heighten counter at button press
  observeEvent(input$onesamplebtn,ignoreInit = TRUE,{
    temp <- counter() + 1
    counter(temp)
  })

  #Define plotdf to store plotting data for later plotting
  plotdf <- numeric()
  
  output$mainplot <- renderPlot({
    # Take 100 samples if counter is one and store in plotdf
    if(counter() == 1){
      samples <- replicate(N,rnorm(n,mean,sd))
      means <- apply(samples,MARGIN = 2, mean)
      sds <- apply(samples, MARGIN = 2, sd)
      ses <- qt(1 - ( 1 - alpha ) / 2, df = (n - 1))*sds/sqrt(n)
      lower <- means - ses
      upper <- means + ses
      contained <- as.factor(ifelse(mean > lower & mean < upper, "TRUE", "FALSE"))
      plotdf <<- data.frame(y = 1:N, means,lower,upper,contained = as.factor(contained))
    }
    # Show the samples of the data frame by counter
    if(counter() >= 1 && counter() <= 5){
      df <- plotdf[1:counter(), ] 
    } 
    #Switch button to take 95 samples at 5
    if(counter() == 5){
      updateActionButton(session,"onesamplebtn", label = "Take 95 more samples")
    }
    #Switch button text to reset at 6
    if(counter() == 6){
      df <- plotdf
      updateActionButton(session,"onesamplebtn", label = "Reset")
    } 
    #Return to Take another sample and reset counter to 1
    if(counter() == 7) {
      updateActionButton(session,"onesamplebtn", label = "Take another sample")
      counter(1)
    }
    

    #MAIN PLOT
    ggplot(df,aes(x = means,y = y, colour = contained)) +
      geom_point() +
      geom_vline(xintercept = mean) + 
      geom_segment(aes(x = lower,
                       xend = upper,
                       y = y,
                       yend = y,
                       colour = contained)
                   ) + 
      ylab("Sample") + 
      xlab("Mean") + 
      coord_cartesian(xlim = c(2,4),
                      ylim = c(0, 100)) + 
      scale_colour_manual(values = c("FALSE" = unname(brewercolors["Red"]),"TRUE" = unname(brewercolors["Blue"])),
                          breaks = c("FALSE", "TRUE"), labels = c("FALSE", "TRUE"), name = "True mean in interval?", drop = FALSE) +
      theme_general() + 
      theme(legend.position = "bottom")
    
  })
  
})
