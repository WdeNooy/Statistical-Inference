library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  # TEXT/LABEL ELEMENT POSITIONS
  elpos <- data.frame(x = c(.15,.5,.5,.5,.5,.85), y = c(.15,.55,.75,.95,1.15,.15))
  
  # LABEL TEXT
  labels <- c(" Pol interest ",
              "        Age        ",
              " News site use", 
              "   Education   ",
              " Pol cynicism ",
              "Reading time"
  )
  # POSSIBLE LABEL COLOURS
  labelcolswitch <- list(none = c("black","darkgrey","darkgrey","darkgrey","darkgrey","black"),
                         age = c("black","black","darkgrey","darkgrey","darkgrey","black"),
                         news = c("black","darkgrey","black","darkgrey","darkgrey","black"),
                         edu = c("black","darkgrey","darkgrey","black","darkgrey","black"),
                         polcyn = c("black","darkgrey","darkgrey","darkgrey","black","black"))
  # ARROW POSITIONS
  arrowpos <- data.frame(x = c(.15,.15,.15,.15,.66,.66,.66,.66),
                         xend = c(.34,.34,.34,.34,.85,.85,.85,.85),
                         y = c(.15,.15,.15,.15,.5,.71,.91,1.1),
                         yend = c(.5,.71,.91,1.1,.2,.2,.2,.2))
  # POSSIBLE ARROW COLOURS
  arrcolswitch <-
    list(
      none = rep("darkgrey", times = 8),
      age = c("blue", "darkgrey", "darkgrey","darkgrey", "blue", "darkgrey", "darkgrey","darkgrey"),
      news = c("darkgrey", "blue", "darkgrey","darkgrey", "darkgrey", "red", "darkgrey","darkgrey"),
      edu = c("darkgrey", "darkgrey", "blue","darkgrey", "darkgrey", "darkgrey", "red","darkgrey"),
      polcyn = c("darkgrey", "darkgrey","darkgrey","red", "darkgrey", "darkgrey","darkgrey", "red")
    )
  
  # ARROW TEXT LABEL POSITIONS
  arrowlabpos <- data.frame(x = c(.38,.38,.38,.38,.62,.62,.62,.62),
                            y = c(.46,.67,.875,1.05,.46,.67,.875,1.05)
  )
  # ARROW TEXT LABEL LABELS
  arrlablabs <- c(0.12, 0.01,0.28,-0.18,0.88,-0.84,-0.12,-0.14)
  
  # POSSIBLE ARROW TEXT LABEL COLURS
  arrlabcolswitch <-  list(
    none = rep("darkgrey", times = 8),
    age = c("blue", "darkgrey", "darkgrey","darkgrey", "blue", "darkgrey", "darkgrey","darkgrey"),
    news = c("darkgrey", "blue", "darkgrey","darkgrey", "darkgrey", "red", "darkgrey","darkgrey"),
    edu = c("darkgrey", "darkgrey", "blue","darkgrey", "darkgrey", "darkgrey", "red","darkgrey"),
    polcyn = c("darkgrey", "darkgrey","darkgrey","red", "darkgrey", "darkgrey","darkgrey", "red")
  )
  
  # FUNCTION FOR CALCULATING LINE WIDTH
  lwdfunc <- function(sbeta){
    if(sbeta == 0) return(0)
    if(abs(sbeta) >= 1.5) return(return(0.3 * abs(1.5) * 10 + 0.01 * abs(sbeta) * 10))
    else(return(0.5 * abs(sbeta) * 10))
  }
  # ARROW SIZE 
  arrsize <- numeric()
  for (i in arrlablabs) arrsize <- c(arrsize,lwdfunc(i))
  
  #ARROW SIZES FOR DIRECT INDIRECT
  #MAIN PLOT
  output$mainplot <- renderPlot({
    # Switch for arrow colours
    arrowcolour <- switch(input$confradbut,
                          none = arrcolswitch$none,
                          news = arrcolswitch$news,
                          age = arrcolswitch$age,
                          edu = arrcolswitch$edu,
                          polcyn = arrcolswitch$polcyn
    )
    # Switch for label colour
    labelcolour <- switch(input$confradbut,
                          none = labelcolswitch$none,
                          news = labelcolswitch$news,
                          age = labelcolswitch$age,
                          edu = labelcolswitch$edu,
                          polcyn = labelcolswitch$polcyn
    )
    # Switch for arrow label colour
    arrlabcolour <- switch(input$confradbut,
                           none = arrlabcolswitch$none,
                           news = arrlabcolswitch$news,
                           age = arrlabcolswitch$age,
                           edu = arrlabcolswitch$edu,
                           polcyn = arrlabcolswitch$polcyn
    )
    # Hardcode of partial label/size
    partialsize <- switch(input$confradbut,
                          none = NA,
                          news = 0.27,
                          age =  0.09,
                          edu = 0.18,
                          polcyn = 0.12)
    
    #MAIN PLOT                      
    ggplot(elpos, aes(x = x, y = y)) +
      #All segments/arrows running via the mediator
      geom_segment(data = arrowpos, aes(x = x, y = y, xend = xend, yend = yend),
                   colour = arrowcolour, alpha = .8,
                   size = arrsize) + 
      #Simple arrow
      geom_segment(x = .15,
                   xend = .7,
                   y = .15,
                   yend = .15,
                   alpha = .4,
                   arrow = arrow(length = unit(0.03, "npc"),type = "closed"),
                   size = lwdfunc(0.14)) + 
      #Labels/Elements
      geom_label(aes(x = x, y = y),
                 label = labels,
                 colour = labelcolour,
                 fill = "white",
                 label.r = unit(0,"lines"),
                 label.padding = unit(.4, "lines"),
                 size  = 5) +
      # Arrow labels
      geom_text(data = arrowlabpos,
                aes(x = x, y = y),
                label = arrlablabs,
                colour = arrlabcolour) +
      # Text label Simple
      geom_text(x = .5, y = .2, label = "Simple: 0.14") +
      # Text label Partial
      geom_text(x = .5, y = .1, 
                label = ifelse(is.na(partialsize), "", paste("Partial:",partialsize)),
                colour = "blue") + 
      # Coordinate definitions
      scale_x_continuous(limits = c(0,1), breaks = NULL) +
      scale_y_continuous(limits = c(0,1.18), breaks = NULL) +
      xlab("") +
      ylab("") +
      theme_general()
  })
  
})
