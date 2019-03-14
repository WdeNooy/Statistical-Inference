library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  # TEXT/LABEL ELEMENT Y POSITIONS and LABEL TEXT
  labels <- data.frame(y = c(.15,.55,.75,.95,.15),
                      label = c(" Pol interest ",
                                  "        Age        ",
                                  " News site use", 
                                  "   Education   ",
                                  "Reading time"
                      ),
                      stringsAsFactors = FALSE) #we need a df for ggplot

  # LABEL X POSITIONS
  labxswitch <- list(none = c(.15,.5,.5,.5,.85),
                     age = c(.15,.15,.5,.5,.85),
                     news = c(.15,.5,.15,.5,.85),
                     edu = c(.15,.5,.5,.15,.85))
  
  # POSSIBLE LABEL COLOURS
  labelcolswitch <- list(none = c("black","darkgrey","darkgrey","darkgrey","black"),
                         age = c("black","black","darkgrey","darkgrey","black"),
                         news = c("black","darkgrey","black","darkgrey","black"),
                         edu = c("black","darkgrey","darkgrey","black","black"))
  # ARROW POSITIONS
  # Fixed
  arrowpos <- data.frame(xend = c(.15,.15,.15,.85,.85,.85),
                         yend = c(.15,.15,.15,.2,.2,.2),
                         y = c(.55,.75,.95,.55,.75,.95))
  # Variable
  arrowxswitch <- list(none = c(.36,.36,.36,.64,.64,.64),
                       age = c(.15,.36,.36,.26,.64,.64),
                       news = c(.36,.15,.36,.64,.26,.64),
                       edu = c(.36,.36,.15,.64,.64,.26))
  # POSSIBLE ARROW COLOURS
  arrcolswitch <-
    list(
      none = rep("darkgrey", times = 6),
      age = c("blue", "darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey"),
      news = c("darkgrey", "blue", "darkgrey", "darkgrey", "blue", "darkgrey"),
      edu = c("darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey", "blue")
    )
  
  # ARROW TEXT LABEL POSITIONS
  arrowlabpos <- data.frame(y = c(.46,.67,.88,.46,.67,.88))
  # ARROW X POSITIONS
  arrowlabxswitch <- list(none = c(.38,.38,.38,.62,.62,.62),
                          age = c(.10,.38,.38,.54,.62,.62),
                          news = c(.38,.10,.38,.62,.44,.62),
                          edu = c(.38,.38,.10,.62,.62,.39))
  # ARROW TEXT LABEL LABELS
  arrlablabs <- c(0.12, 0.01,0.28,0.88,-0.84,-0.12)
  
  # POSSIBLE ARROW TEXT LABEL COLOURS
  arrlabcolswitch <-  list(
    none = rep("darkgrey", times = 6),
    age = c("blue", "darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey"),
    news = c("darkgrey", "blue", "darkgrey", "darkgrey", "blue", "darkgrey"),
    edu = c("darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey", "blue")
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
   # Switch for label positions
   labx <- switch(input$confradbut,
                  none = labxswitch$none,
                  news = labxswitch$news,
                  age = labxswitch$age,
                  edu = labxswitch$edu
            )
   # Switch for label colour
   labelcolour <- switch(input$confradbut,
                         none = labelcolswitch$none,
                         news = labelcolswitch$news,
                         age = labelcolswitch$age,
                         edu = labelcolswitch$edu
                  )
   # Switch for arrow x end positions
   arrowposx <- switch(input$confradbut,
                         none = arrowxswitch$none,
                         news = arrowxswitch$news,
                         age = arrowxswitch$age,
                         edu = arrowxswitch$edu
   )
   # Switch for arrow label x positions
   arrowlabx <- switch(input$confradbut,
                       none = arrowlabxswitch$none,
                       news = arrowlabxswitch$news,
                       age = arrowlabxswitch$age,
                       edu = arrowlabxswitch$edu
   )
   # Switch for arrow colours
   arrowcolour <- switch(input$confradbut,
                         none = arrcolswitch$none,
                         news = arrcolswitch$news,
                         age = arrcolswitch$age,
                         edu = arrcolswitch$edu
   )
   # Switch for arrow label colour
   arrlabcolour <- switch(input$confradbut,
                          none = arrlabcolswitch$none,
                          news = arrlabcolswitch$news,
                          age = arrlabcolswitch$age,
                          edu = arrlabcolswitch$edu
                    )
   # Hardcode of partial label/size
   partialsize <- switch(input$confradbut,
                         none = NA,
                         news = 0.15,
                         age =  0.04,
                         edu = 0.19)
                          
    #MAIN PLOT                      
    ggplot(labels) +
      #All segments/arrows running via the mediator
      geom_segment(data = arrowpos, aes(x = arrowposx, y = y, xend = xend, yend = yend),
                   colour = arrowcolour, alpha = .8,
                   size = arrsize) + 
      #Partial arrow
      # geom_segment(x = .15,
      #              xend = .7,
      #              y = .15,
      #              yend = .15,
      #              alpha = .7,
      #              colour = "blue",
      #              arrow =arrow(length = unit(0.03, "npc"),type = "closed"),
      #              size = lwdfunc(partialsize)*2) +
      #Simple arrow
      geom_segment(x = .15,
                   xend = .7,
                   y = .15,
                   yend = .15,
                   alpha = .4,
                   arrow = arrow(length = unit(0.03, "npc"),type = "closed"),
                   size = lwdfunc(0.14)) + 
      #Labels/Elements
      geom_label(aes(x = labx, y = y),
                 label = labels$label,
                 colour = labelcolour,
                 fill = "white",
                 label.r = unit(0,"lines"),
                 label.padding = unit(.4, "lines"),
                 size  = 5) +
      # Arrow labels
      geom_text(data = arrowlabpos,
                aes(x = arrowlabx, y = y),
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
      scale_y_continuous(limits = c(0,1), breaks = NULL) +
      xlab("") +
      ylab("") +
      theme_general()
  })
  
})
