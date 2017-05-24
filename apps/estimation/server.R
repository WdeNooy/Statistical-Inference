library(shiny)
library(ggplot2)

shinyServer(function(input, output,session) {

  source("../plottheme/styling.R", local = TRUE)
  
  mean <- 2.8 #Population mean
  sd <- 0.5 #Population sd
  nmax = 30 # Maximal n value
  nmin = 1 # Minimal n value
  
  ###### CONNECTION BETWEEN BOTH SLIDERS#########
  
  #If nslider moves, change seslider
  observeEvent(input$nslider,{
        updateSliderInput(session, inputId = "seslider",
                          value = input$nslider - 1
                          )
  })
  
  #If seslider moves, change nslider
  observeEvent(input$seslider,{
    updateSliderInput(session,
                      inputId = "nslider",
                      value = input$seslider + 1
                     )
  })

  #######################   MAIN PLOT #############################
    output$mainplot <- renderPlot({
      #Validations
      validate(
        need(
          input$cfintslider != 100,
          "Selecting 100% leads to an infinitely wide confidence interval"
        )
      )
      
      tailarea <-
        (1 - input$cfintslider / 100) / 2 #calculates value for qnorm
      
      
      posvalues <- sd / sqrt(nmin:nmax) #possible values standard error can take
      stderr <- posvalues[input$seslider + 1] #stderror
      

    
    dist <- qnorm(1 - tailarea) * stderr #Distance for interval  
    left <- mean - dist #Left confidence interval border
    right <- mean + dist #Right confidence interval border
    ##### PLOT ######
    ggplot(data.frame(x = c(0,6)), aes(x = x)) + 
      #Normal distribution
      stat_function(fun = dnorm, args = list(mean = mean, sd = stderr)) + 

      #X scale definition and generation of z score scale on top
      scale_x_continuous(breaks = seq(0,6,by = .1),limits = c(2.4,3.2),
                        
                         sec.axis = sec_axis((~./stderr - mean/stderr),
                                              breaks = -4:4,
                                              name = "Standard error (z)")) +
      #Left vline
      geom_vline(aes(xintercept = left,
                     linetype = "left margin")) +
      #Right vline
      geom_vline(aes(xintercept = right,
                     linetype = "right margin")) +
      #Mean vline
      geom_vline(aes(xintercept = mean,
                     linetype = "mean")) + 
      #Defining legend of lines
      scale_linetype_manual(name = "",
                            values = c("left margin" = "dashed",
                                       "right margin" = "dashed",
                                       "mean" = "solid")) + 
      #Center text label
      geom_text(label = paste(input$cfintslider, "%", sep = ""),
                aes(x=mean,
                    y = dnorm(mean, mean = mean, sd = sd)/2,
                    vjust = .5
                )) +
      #Left text label
      geom_text(label = paste((100 - input$cfintslider)/2, "%", sep = ""),
                aes(x=left,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 1)) +
      #Right text label
      geom_text(label = paste((100 - input$cfintslider)/2, "%", sep = ""),
                aes(x=right,
                    y = dnorm(mean, mean = mean, sd = sd)/3,
                    hjust = 0)) +
      #Left arrow
      geom_segment(aes(x = mean,
                       xend = left,
                       y = dnorm(mean, mean = mean, sd = sd) * .8,
                       yend = dnorm(mean, mean = mean, sd = sd) * .8,
                       colour = "Interval estimate"), 
                   size = 1,
                   arrow = arrow(length = unit(.3,"cm"))) + 
      #Right arrow
      geom_segment(aes(x = mean,
                       xend = right,
                       y = dnorm(mean, mean = mean, sd = sd) * .8,
                       yend = dnorm(mean, mean = mean, sd = sd) * .8,
                       colour =  "Interval estimate"), 
                   size = 1,
                   arrow = arrow(length = unit(.3,"cm"))) +
      
      #Specifing legend for colour/arrow
      scale_colour_manual(guide = guide_legend(title = ""),
                          values = c("Interval estimate" = unname(brewercolors["Red"]))) + 
      ggtitle("Sampling distribution") + 
      ylab("Density") +
      xlab("Candy weight") +
      #general theme
      theme_general() +
      theme(legend.position = "bottom")
  })

})
