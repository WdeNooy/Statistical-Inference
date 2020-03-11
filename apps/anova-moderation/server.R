library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  #Load styling files for plots
  source("../plottheme/styling.R", local = TRUE)
  
  #Reactive data frame for plotting the means
  data <- reactive(data.frame(endorser = factor(c("Clooney","Jolie","No endorser","Clooney","Jolie","No endorser"),
                                                levels = c("Clooney","Jolie","No endorser")),
                              sex = as.factor(c(rep("male", 3), rep("female", 3))),
                              willingness_av = c(input$menclooney,
                                                 input$menjolie,
                                                 input$mennobody,
                                                 input$woclooney,
                                                 input$wojolie,
                                                 input$wonobody)))

  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    #Load data
    df <- data()
   
     #Validation block to check if input within range of 0 and 10
    validate(
      need(!(input$wonobody < 0 || input$wonobody > 10),
            "Please use values between 0 and 10"),
      need(!(input$woclooney < 0 || input$woclooney > 10),
           "Please use values between 0 and 10"),
      need(!(input$wojolie < 0 || input$wojolie > 10),
           "Please use values between 0 and 10"),
      need(!(input$mennobody < 0 || input$mennobody > 10),
           "Please use values between 0 and 10"),
      need(!(input$menclooney < 0 || input$menclooney > 10),
           "Please use values between 0 and 10"),
      need(!(input$menjolie < 0 || input$menjolie > 10),
           "Please use values between 0 and 10")
      )

    #Dataframe containing vertical arrow positions 
    dfarrows <- data.frame(y = df$willingness_av[1:3],
                           yend  = df$willingness_av[4:6])
    #Add space at the right side.
    dfarrows$y <- ifelse(dfarrows$y > dfarrows$yend,
                         dfarrows$y - 0.2,
                         dfarrows$y + 0.2)
    dfarrows$yend <- ifelse(dfarrows$y > dfarrows$yend,
                         dfarrows$yend + 0.2,
                         dfarrows$yend - 0.2)

    #Plot output
    ggplot(df, aes(x = endorser,
                  y = willingness_av,
                  group = sex,
                  colour = sex)) +
      #Plot the means by endorsers
      geom_point(aes(shape = sex),
                 size = 3) + 
      #Connect the means with lines
      geom_line(aes(linetype = sex),
                size = .7) + 
      #Add vertical arcs between subgroup means
      geom_segment(x = 1,
                   xend = 1,
                   y = dfarrows$y[1],
                   yend = dfarrows$yend[1],
                   linetype = "solid",
                   color = brewercolors["Green"],
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "last",
                                 type = "closed")) + 
      geom_segment(x = 2,
                   xend = 2,
                   y = dfarrows$y[2],
                   yend = dfarrows$yend[2],
                   linetype = "solid",
                   color = brewercolors["Green"],
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "last",
                                 type = "closed")) + 
      geom_segment(x = 3,
                   xend = 3,
                   y = dfarrows$y[3],
                   yend = dfarrows$yend[3],
                   linetype = "solid",
                   color = brewercolors["Green"],
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "last",
                                 type = "closed")) + 
      #Set limits of plot
      scale_y_continuous(limits = c(0,10),
                         breaks = seq(1,10)) +
      #Set colours per condition
      scale_color_manual(values = c(unname(brewercolors["Red"]),
                                    unname(brewercolors["Blue"]))) + 
      #Labels and title
      ylab("Willingness") + 
      xlab("Endorser") + 
      ggtitle("Average willingness by endorser") + 
      #Theme settings
      theme_general() + 
      theme(legend.position = "bottom")
  })
  
  #Output for total of Nobody
  output$totnobtext <- renderText(
    as.character(
      round(
        mean(
          c(input$wonobody, input$mennobody)
        ),
        digits = 2
      )
    )
  )
  #Output for total clooney
  output$totclotext <- renderText(
    as.character(
      round(
        mean(
          c(input$woclooney, input$menclooney)
        ),
        digits = 2
      )
    )
  )
  #Output for total jolie
  output$totjoltext <- renderText(
    as.character(
      round(
        mean(
          c(
            input$wojolie, input$menjolie)
        ),
        digits = 2
      )
    )
  )
  #Output for total women
  output$totwomtext <- renderText(
    as.character(
      round(
        mean(c(input$wonobody,
               input$woclooney,
               input$wojolie)
        ),
        digits = 2
      )
    )
  )
  #Output for total men
  output$totmentext <- renderText(
    as.character(
      round(
        mean(
          c(input$mennobody,
            input$menclooney,
            input$menjolie)
        ),
        digits = 2
        
      )  
    )
  )
  #Output for total of all cells
  output$tottext <- renderText(
    as.character(
      round(
        mean(
          c(input$mennobody,
            input$menclooney,
            input$menjolie,
            input$wonobody,
            input$woclooney,
            input$wojolie)
        ),
        digits = 2
      )
    )
  )
})
