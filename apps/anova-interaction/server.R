library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  #Load styling files for plots
  source("../plottheme/styling.R", local = TRUE)
  
  #Reactive data frame for plotting the means
  data <- reactive(data.frame(endorser = factor(c("Nobody","Clooney","Jolie","Nobody","Clooney","Jolie"),
                                                levels = c("Nobody","Clooney","Jolie")),
                              sex = as.factor(c(rep("male", 3), rep("female", 3))),
                              willingness_av = c(input$mennobody,
                                                 input$menclooney,
                                                 input$menjolie,
                                                 input$wonobody,
                                                 input$woclooney,
                                                 input$wojolie)))
  #Calculations for p value, f value and eta^2
  stats <- reactive({df <- data()
                    n <- 10 # size of each group
                    SD <- 1.5 #Sd of each group
                    k <- 6 #cells
                    
                    #Averages/Means in cells for calculating interaction
                    avnob <- mean(df$willingness_av[df$endorser == "Nobody"])
                    avcloon <- mean(df$willingness_av[df$endorser == "Clooney"])
                    avjol <- mean(df$willingness_av[df$endorser == "Jolie"])
                    avmen <-mean(df$willingness_av[df$sex == "male"])
                    avwom <- mean(df$willingness_av[df$sex == "female"])
                    avtot <- mean(df$willingness_av)
                    #SS of interaction
                    SSsexend <- ((input$mennobody - avmen - avnob + avtot)^2 +
                                   (input$menclooney - avmen - avcloon + avtot)^2 +
                                   (input$menjolie - avmen - avjol + avtot)^2 +
                                   (input$wonobody - avwom -avnob + avtot)^2 +
                                   (input$woclooney - avwom - avcloon + avtot)^2 +
                                   (input$wojolie - avwom - avjol + avtot)^2) * k
                    #Calculated by assuming that all groups have the same SD.
                    SSwith <- (SD^2 * n) * k
                    #Calculated by assuming a df of 2 [(a-1)*(b-1)]
                    MSsexend <- SSsexend/2
                    #Calculated by assuming a total N of 60.
                    MSwith <-SSwith/((n * k) - k)
                    
                    fval <- MSsexend/MSwith # F value
                    
                    #P value 
                    pval <- pf(fval,
                               df1 = 2,
                               df2 = (n * k)- k,
                               lower.tail = FALSE
                               )
                    
                    eta <- SSsexend/SSwith # Eta
                    # Return named vector
                    c("pval" = pval, "fval" = fval, "eta" = eta)})
  
  #Output of results (p,f,eta) in mathjax format
  output$fvaltext <- renderUI({
    withMathJax(
      helpText(
        paste("$$F(2,54) = ",
               round(stats()["fval"],3),
              "\\;P = ",
              round(stats()["pval"],3),
              "$$"),
        paste("$$\\eta^2 = ",
              round(stats()["eta"],3),
              "$$"
              )
        )
      )
  })
  
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
    
    #Plot output
    ggplot(df, aes(x = endorser,
                  y = willingness_av,
                  group = sex,
                  colour = sex)) +
      #Plot the means by endorses
      geom_point(aes(shape = sex),
                 size = 3) + 
      #Connect the means with lines
      geom_line(aes(linetype = sex),
                size = .7) + 
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
  
  #Output for total of nobody
  output$totnobtext <- renderText(
    as.character(
      round(
        mean(
          c(input$wonobody, input$mennobody)
        ),
        digits = 3
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
        digits = 3
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
        digits = 3
      )
    )
  )
  #Output for total men
  output$totwomtext <- renderText(
    as.character(
      round(
        mean(c(input$wonobody,
               input$woclooney,
               input$wojolie)
        ),
        digits = 3
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
        digits = 3
        
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
        digits = 3
      )
    )
  )
})
