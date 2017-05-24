library(shiny)
library(ggplot2)
library(RColorBrewer)

#Load general plot theme and colors for color brewer
source("../plottheme/styling.R",local = TRUE)

shinyServer(function(input, output) {

    #Store expectedvalueplot
  expectedvalueplot <- renderPlot({
    df <- data.frame("Number.of.Yellow.candies" = 0:10,
                     Probability = round(dbinom(0:10, 10, input$probslider), 3))
    
    wMean <-
      weighted.mean(x = df$Number.of.Yellow.candies, df$Probability)
    
    ggplot(df, aes(x = Number.of.Yellow.candies, y = Probability)) +
      geom_bar(stat = "identity", fill = brewercolors["Yellow"]) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
      scale_x_continuous(limits = c(-.5, 10.5), breaks = 0:10) + 
      xlab("Number of yellow candies") +
      theme_general()

  })
  #Create an additional trigger for changes in the slider
  observeEvent(input$probslider, {
    output$expectedvalueplot <- expectedvalueplot
    output$answertext <- renderText("")
  })
  #Once submit button is pressed, evaluate answer and print mean in plot if
  #correct;
  observeEvent(input$submit, {
    df <- data.frame("Number.of.Yellow.candies" = 0:10,
                     Probability = round(dbinom(0:10, 10, input$probslider), 3))
    wMean <-
      weighted.mean(x = df$Number.of.Yellow.candies, df$Probability)
    #Give feedback on answer and plot mean if answer is correct.
    if (input$answer == round(wMean, 0)) {
      output$answertext <- renderText("This is the correct answer!")
      
      output$expectedvalueplot <<- renderPlot({
        ggplot(df, aes(x = Number.of.Yellow.candies, y = Probability)) +
          geom_bar(stat = "identity", fill = brewercolors["Yellow"]) +
          geom_vline(
            aes(xintercept = wMean),
            color = brewercolors["Red"],
            linetype = "dashed",
            size = 1
          ) +
          geom_text(aes(
            x = wMean,
            y = .8,
            label = paste("Mean:", as.character(round(wMean))),
            hjust = -0.2
          ), color = brewercolors["Red"]) +
          scale_y_continuous(limits = c(0, 1),
                             breaks = seq(0, 1, by = .2)) +
          scale_x_continuous(limits = c(-.5, 10.5), breaks = 0:10) + 
          xlab("Number of yellow candies") + 
          theme_general()
      })
    }
    else {
      output$answertext <- renderText("This is not the correct answer.")
    }
  })
  
  
})
