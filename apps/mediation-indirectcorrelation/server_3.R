library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  # DATA FOR LABELS: ONE DATA FRAME PER SCENARIO
  lab_none <- data.frame(label = c(" Pol interest ",
                                   "        Age        ",
                                   " News site use", 
                                   "   Education   ",
                                   "Reading time"),
                         x = c(.15,.5,.5,.5,.85),
                         y = c(.15,.55,.75,.95,.15),
                         color = c("black","darkgrey","darkgrey","darkgrey","black"),
                         stringsAsFactors = FALSE)
  lab_age <- data.frame(label = c(" Pol interest ",
                                   "        Age        ",
                                   " News site use", 
                                   "   Education   ",
                                   "Reading time"),
                        x = c(.15,.15,.5,.5,.85),
                        y = c(.15,.55,.75,.95,.15),
                        color = c("black","black","darkgrey","darkgrey","black"),
                        stringsAsFactors = FALSE)
  lab_news <- data.frame(label = c(" Pol interest ",
                                  "        Age        ",
                                  " News site use", 
                                  "   Education   ",
                                  "Reading time"),
                        x = c(.15,.5,.15,.5,.85),
                        y = c(.15,.55,.75,.95,.15),
                        color = c("black","darkgrey","black","darkgrey","black"),
                        stringsAsFactors = FALSE)
  lab_edu <- data.frame(label = c(" Pol interest ",
                                  "        Age        ",
                                  " News site use", 
                                  "   Education   ",
                                  "Reading time"),
                        x = c(.15,.5,.5,.15,.85),
                        y = c(.15,.55,.75,.95,.15),
                        color = c("black","darkgrey","darkgrey","black","black"),
                        stringsAsFactors = FALSE)

  # FUNCTION FOR CALCULATING LINE WIDTH
  lwdfunc <- function(sbeta){
    arrsize <- numeric()
    for (i in 1:length(sbeta)) {
      arrsize <- c(arrsize, 
                   ifelse(sbeta[i] == 0, 0, 
                   ifelse(abs(sbeta[i]) >= 1.5, 
                   0.3 * abs(1.5) * 10 + 0.01 * abs(sbeta[i]) * 10, 
                   0.5 * abs(sbeta[i]) * 10)))
    }
    return(arrsize)
  }
  
  # DATA FOR LINES: ONE DATA FRAME PER SCENARIO
  line_none <- data.frame(x = c(.40,.40,.40,.60,.60,.60),
                          xend = c(.15,.15,.15,.85,.85,.85),
                          y = c(.55,.75,.95,.55,.75,.95), 
                          yend = c(.15,.15,.15,.2,.2,.2),
                          color = rep("darkgrey", times = 6), #line and label
                          label = c(0.12, 0.01,0.28,0.88,-0.84,-0.12),
                          xlab = c(.41,.41,.41,.59,.59,.59),
                          ylab = c(.46,.67,.88,.46,.67,.88),
                          width = lwdfunc(c(0.12, 0.01,0.28,0.88,-0.84,-0.12)),
                          arrowsize = c(0, 0, 0, 0, 0, 0))
  line_age <- data.frame(x = c(.15,.40,.40,.24,.60,.60),
                          xend = c(.15,.15,.15,.85,.85,.85),
                          y = c(.55,.75,.95,.55,.75,.95), 
                          yend = c(.15,.15,.15,.2,.2,.2),
                          color = c("blue", "darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey"),
                          label = c(0.12, 0.01,0.28,0.83,-0.84,-0.12),
                          xlab = c(.10,.41,.41,.54,.59,.59),
                          ylab = c(.46,.67,.88,.46,.67,.88),
                          width = lwdfunc(c(0.12, 0.01,0.28,0.83,-0.84,-0.12)),
                          arrowsize = c(0, 0, 0, 0.2, 0, 0))
  line_news <- data.frame(x = c(.40,.15,.40,.60,.24,.60),
                         xend = c(.15,.15,.15,.85,.85,.85),
                         y = c(.55,.75,.95,.55,.75,.95), 
                         yend = c(.15,.15,.15,.2,.2,.2),
                         color = c("darkgrey", "blue", "darkgrey", "darkgrey", "blue", "darkgrey"),
                         label = c(0.12, 0.01,0.28,0.88,-0.79,-0.12),
                         xlab = c(.41,.10,.41,.59,.44,.59),
                         ylab = c(.46,.67,.88,.46,.67,.88),
                         width = lwdfunc(c(0.12, 0.01,0.28,0.88,-0.79,-0.12)),
                         arrowsize = c(0, 0, 0, 0, 0.2, 0))
  line_edu <- data.frame(x = c(.40,.40,.15,.60,.60,.24),
                         xend = c(.15,.15,.15,.85,.85,.85),
                         y = c(.55,.75,.95,.55,.75,.95), 
                         yend = c(.15,.15,.15,.2,.2,.2),
                         color = c("darkgrey", "darkgrey", "blue", "darkgrey", "darkgrey", "blue"),
                         label = c(0.12, 0.01,0.28,0.88,-0.84,-0.18),
                         xlab = c(.41,.41,.10,.59,.59,.39),
                         ylab = c(.46,.67,.88,.46,.67,.88),
                         width = lwdfunc(c(0.12, 0.01,0.28,0.88,-0.84,-0.18)),
                         arrowsize = c(0, 0, 0, 0, 0, 0.05))
  
  #MAIN PLOT
  output$mainplot <- renderPlot({
   # Hardcode of partial label/size
   partialsize <- switch(input$confradbut,
                         none = NA,
                         news = 0.15,
                         age =  0.04,
                         edu = 0.19)
   # Select requested data
   labels <-  switch(input$confradbut,
                none = lab_none,
                news = lab_news,
                age =  lab_age,
                edu = lab_edu)
   lines <-  switch(input$confradbut,
                     none = line_none,
                     news = line_news,
                     age =  line_age,
                     edu = line_edu)
   
    #MAIN PLOT                      
    ggplot(labels) +
      #All lines linked to a confounder
      geom_segment(data = lines, 
                   aes(x = x, y = y, xend = xend, yend = yend), 
                   size = lines$width,
                   colour = lines$color, 
                   alpha = .8,
                   arrow = arrow(length = unit(lines$arrowsize*0.5, "npc"),type = "closed")) + 
      #Simple arrow
      geom_segment(x = .15,
                   xend = .74,
                   y = .15,
                   yend = .15,
                   alpha = .4,
                   arrow = arrow(length = unit(0.03, "npc"),type = "closed"),
                   size = lwdfunc(0.14)) + 
      #Labels
      geom_label(data = labels, 
                 aes(x = x, y = y, label = label), 
                 colour = labels$color,
                 fill = "white",
                 label.r = unit(0,"lines"),
                 label.padding = unit(.4, "lines"),
                 size  = 5) +
      # Arrow labels
      geom_text(data = lines, 
                aes(x = xlab, y = ylab, label = label), 
                colour = lines$color) +
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
