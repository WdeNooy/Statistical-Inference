library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
# Source styling files    
source("../plottheme/styling.R", local = TRUE)

#read-in csv file 
readers_org <- read.csv(file = "readers.csv")
#mean-center (and standardize) variables, so regression lines are centered
readers <- as.data.frame(lapply(readers_org, scale))

#Extract coefficients and put into data frame for simple model
modelsimple <- data.frame(coef = coef(lm(readingtime ~ polinterest,readers)))
# Idem using original data (otherwise b*)
modelsimple_org <- data.frame(coef = coef(lm(readingtime ~ polinterest,readers_org)))

#Construct label
simplelabel <- paste0("Simple effect of political interest (b* = ", round(modelsimple$coef[2], digits = 2), "); readingtime = constant + polinterest")

output$mainplot <- renderPlot({

  #Formula for full model
  if (length(input$predcheckbox) == 0) {
    formula <- "readingtime ~ polinterest"
    #Define colour label as name
    partiallabel <- "Partial effect of political interest"
  } else {
    formula <- paste("readingtime ~ polinterest + ", paste(input$predcheckbox,collapse = "+"))
    #Define colour label as name
    partiallabel <- paste0("Partial effect of political interest (b* = ", round(coef(lm(eval(formula),readers))[[2]], digits = 2), "); \nreadingtime = constant + polinterest + ", paste(input$predcheckbox,collapse = " + "))
  }
  
  #Extract coefficients and put into data frame
  model <- data.frame(coef = coef(lm(eval(formula),readers)))
  
  #MAIN PLOT
  ggplot(readers, aes(x = polinterest, y = readingtime)) + 
    geom_point(size = 1, alpha = .5) + #dots
    geom_segment(aes(x = min(polinterest),
                     xend = max(polinterest),
                     y = modelsimple$coef[1] + min(polinterest) * modelsimple$coef[2],
                     yend = modelsimple$coef[1] + max(polinterest) * modelsimple$coef[2],
                     color = "Simple effect"),
                 size = 2) +
    {if(length(input$predcheckbox)>0) geom_segment(aes( x = min(polinterest),
                                                        xend = max(polinterest),
                                                        y = model$coef[1] + min(polinterest) * model$coef[2],
                                                        yend = model$coef[1] + max(polinterest) * model$coef[2],
                                                        color = "Partial effect"),
                                                   size = 1.5)} +
    scale_color_manual(values = c("Simple effect" = "grey",
                                  "Partial effect" = unname(brewercolors["Blue"])),
                       limits = c("Simple effect", "Partial effect"),
                       labels = c(simplelabel, partiallabel),
                       guide = guide_legend(nrow = 2,title = "" )) + 
    scale_x_continuous(name = "Political interest", 
                       breaks = c(-2, 0, 2),
                       labels = c("low", "average", 'high')
                       ) +
    scale_y_continuous(name = "Newspaper reading time", 
                       breaks = c(-2, 0, 2),
                       labels = c("low", "average", 'high'),
                       limits = c(-3.5, 2.5)
    ) +
    theme_general() + 
    theme(legend.position = c(0.03,0.1), legend.justification = c(0,0.5), legend.background = element_blank())
  
})

})
