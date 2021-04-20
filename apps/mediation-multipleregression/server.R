library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
# Source styling files    
source("../plottheme/styling.R", local = TRUE)

#read-in csv file 
readers <- read.csv(file = "readers.csv")
readers <- as.data.frame(lapply(readers, scale))

#extract bivariate coefficients
biage <- summary(lm(readingtime ~ age,readers))
biedu <- summary(lm(readingtime ~ education ,readers))
bipint <- summary(lm(readingtime ~ polinterest ,readers))
binsite <- summary(lm(readingtime ~ newssite ,readers))

#beta
bicoefficients <- c(coefficients(biage)[2,1],
                    coefficients(biedu)[2,1],
                    coefficients(bipint)[2,1],
                    coefficients(binsite)[2,1])
# SE
bise <- c(coefficients(biage)[2,2],
         coefficients(biedu)[2,2],
         coefficients(bipint)[2,2],
         coefficients(binsite)[2,2])
# Left and right edges of 95% CI
bileft <- bicoefficients - 1.96 * bise
biright <- bicoefficients + 1.96 * bise

#Standard dataframe
bidf <- data.frame(Predictor = c("Age", "Education", "Pol. Interest", "News site use"),
                   bicoef = bicoefficients,
                   bileft = bileft,
                   biright = biright)


output$mainplot <- renderPlot({

  
  
  if(length(input$predcheckbox) == 0){
  mudf <- data.frame(mucoef = rep(NA,4), muleft = rep(NA,4), muright = rep(NA,4))
  plotdf <- cbind(bidf,mudf)
  
  }

if(length(input$predcheckbox) > 0){

  # Filter to determine which variables have to be NA (hidden) for plotting
  filter <- (c("age", "education","polinterest","newssite") %in% input$predcheckbox)
  
  #Formula for model
  formula <- paste("readingtime ~", paste(input$predcheckbox,collapse = "+"))
  
  #Extract coefficients and put into data frame
  model <- coefficients(summary(lm(eval(formula),readers)))
  mucoef <- model[2:(length(input$predcheckbox) + 1), 1 ]
  muse <- model[2:(length(input$predcheckbox) + 1), 2 ]
  
  mudf <- data.frame(mucoef = rep(NA,4), muleft = rep(NA,4), muright = rep(NA,4))
  
  muleft <- mucoef - 1.96 * muse
  muright <- mucoef + 1.96 * muse
  
  mudf$mucoef[filter] <- mucoef
  mudf$muleft[filter] <- muleft
  mudf$muright[filter] <- muright
  
  plotdf <-  cbind(bidf,mudf)
  
}
     #MAIN PLOT
     ggplot(plotdf, aes(y = Predictor)) + 
     geom_point(aes(x = bicoef, colour = "b* and 95% CI in simple regression"), size = 5, alpha = .8) + 
     geom_segment(aes(y = Predictor,
                      yend = Predictor,
                      x = bileft,
                      xend = biright,
                      color = "b* and 95% CI in simple regression"),
                  size = 2, 
                  alpha = 0.5) + 
     # dashed line to X axis: gray
     geom_segment(aes(y = Predictor,
                      yend = -Inf,
                      x = bicoef, xend = bicoef,
                      color = "b* and 95% CI in simple regression"),
                  linetype = "dotted",
                  na.rm = TRUE) +
     {if(length(input$predcheckbox)>0) geom_point(aes(x = mucoef,
                                                      color = "b* and 95% CI in multiple regression"),
                                                  na.rm = TRUE,
                                                  size = 3)} + 
     {if(length(input$predcheckbox)>0) geom_segment(aes(y = Predictor,
                                                        yend = Predictor,
                                                        x = muleft, xend = muright,
                                                        color = "b* and 95% CI in multiple regression"),
                                                    na.rm = TRUE)} +
     # dashed line to X axis: blue
     {if(length(input$predcheckbox)>0) geom_segment(aes(y = Predictor,
                                                          yend = -Inf,
                                                          x = mucoef, xend = mucoef,
                                                          color = "b* and 95% CI in multiple regression"),
                                                      linetype = "dashed",
                                                      na.rm = TRUE)} +
     scale_color_manual(values = c("b* and 95% CI in simple regression" = "grey" ,
                                   "b* and 95% CI in multiple regression" = unname(brewercolors["Blue"])),
                        limits = c("b* and 95% CI in simple regression",
                                   "b* and 95% CI in multiple regression"),
                        guide = guide_legend(nrow = 2,title = "" )) + 
     geom_vline(xintercept = 0) +
     coord_cartesian(xlim = c(-1,1)) + 
     theme_general() + 
     xlab("Standardized regression coefficient") +
     theme(legend.position = "bottom")
  
})

})
