library(shiny)
library(ggplot2)
library(grid)
shinyServer(function(input, output, session) {
  source("../plottheme/styling.R", local = TRUE)
  #Set slider inputs to random drawn values###
  updateSliderInput(session,
                    "agepolslider",
                    value = round(runif(n = 1, min = -.7, max = .7), digits = 2)
                    )
  updateSliderInput(session,
                    "agereadslider",
                    value = round(runif(n = 1, min = -.7, max = .7), digits = 2)
  )
  updateSliderInput(session,
                    "polreadslider",
                    value = round(runif(n = 1, min = -.7, max = .7), digits = 2)
  )
  
  #Function to calculate line and arrow widths

  ###MAIN PLOT###
  output$mainplot <- renderPlot({
    
    #correlation between Predictor and mediator.
    r_PM <- input$agepolslider
    #correlation between mediator and Outcome.
    r_MO <- input$polreadslider
    #correlation between Predictor and Outcome.
    r_PO <- input$agereadslider
    # Partial standardized regression coefficients.
    b_PM <- r_PM
    b_PO <- round((r_PO - r_PM*r_MO)/(1 - r_PM^2), digits = 2)
    b_MO <- round((r_MO - r_PM*r_PO)/(1 - r_PM^2), digits = 2)
    b_indirect <- round(b_PM * b_MO, digits = 2)
    
    lwdfunc <- function(sbeta){
      if(sbeta == 0) return(0)
      if(abs(sbeta) >= 1.5) return(return(0.3 * abs(1.5) * 10 + 0.01 * abs(sbeta) * 10))
      else(return(0.3 * abs(sbeta) * 10))
    }
    labels <- c("Pol. interest\n(Mediator)","Age\n(Predictor)", "Reading time\n(Outcome)")

    elpos <- data.frame(x = c(.5,.15,.85), y = c(.85,.15,.15))
    ggplot(elpos, aes(x = x, y = y)) +
      geom_point() + 
      #Arrow Age to Pol interest
      geom_segment(aes(x = .15, y = .21, xend = .4, yend = .78),
                   arrow = arrow(length = unit(0.03, "npc")),
                   size = lwdfunc(b_PM)) + 
      #Arrow Age to Reading
      geom_segment(aes(x = x[2], y= y[2], xend = .72, yend = y[3]),
                       arrow = arrow(length = unit(0.03, "npc")),
                   size = lwdfunc(b_PO)) + 
      #Arrow Pol interest to Reading
      geom_segment(aes(x = .6, y= .78, xend = .85, yend = .22),
                       arrow = arrow(length = unit(0.03, "npc")),
                   size = lwdfunc(b_MO)) + 
      #Curve Age to Reading
      geom_curve(aes(x = .15, y= .21, xend = .78, yend = .26),
                 curvature = -0.5,
                 linetype = "dashed",
                 color = "red",
                 size = lwdfunc(b_indirect)) +
      #Arrowhead of curve
      geom_curve(aes(x = .77, y= .24, xend = .80, yend = .23),
                 arrow = arrow(length = unit(0.04, "npc"),type = "open"),
                 curvature = -0.5,
                 linetype = "solid",
                 color = "red",
                 size = lwdfunc(b_indirect)) + 
      geom_label(aes(x = x, y = y), label = labels, colour = "black", fill = "white") +
      annotate("text", x = .15, y = .5,
               label = deparse(bquote("b*"[PM] == .(b_PM))),
               parse = TRUE) +
      annotate("text", x = .5, y = .1,
               label = deparse(bquote("b*"[PO] == .(b_PO))),
               parse = TRUE) +
      annotate("text", x = .85, y = .5,
               label = deparse(bquote("b*"[MO] == .(b_MO))),
               parse = TRUE) +
      annotate("text", x = .5, y = .5,
               label = deparse(bquote("b*"[indirect] == .(b_indirect))),
               parse = TRUE,
               colour = "red") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
      theme_general() +
      scale_x_continuous(name = "", breaks = NULL) +
      scale_y_continuous(name = "", breaks = NULL)

  })
})
