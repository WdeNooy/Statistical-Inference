library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  
  #Load styling for plots
  source("../plottheme/styling.R", local = TRUE)
  
  #CREATE PREDICTOR
  n <- 150 #Decreased sample size for more nuanced p values
  
  data <- reactiveValues(
    smokestatus = numeric(),
    attitude = numeric(0)
  )
  
  observeEvent(input$samplebutton,ignoreNULL = FALSE,{
    situation <-sample(1:4,1)
    
    data$smokestatus <- factor(sample(c("Non-smoker","Former smoker", "Smoker"),size = n, replace = TRUE), 
                               levels = c("Non-smoker","Former smoker", "Smoker")) #ref cat = never smoked
    data$smokestatus2 <- data$smokestatus
    contrasts(data$smokestatus2) <- contr.treatment(3, base = 2) #ref cat = stopped smoking
    
    neversdum <- ifelse(data$smokestatus == "Non-smoker", 1, 0) 
    ssdum <- ifelse(data$smokestatus == "Former smoker", 1, 0)
    sdum <- ifelse(data$smokestatus == "Smoker", 1,0)
    if(situation == 1){
      nevers = neversdum * rnorm(n, -1.7, 1) 
      ss = ssdum * rnorm(n, 0.75, 1)
      s = sdum * rnorm(n, 0.75,  1)
    }
    if(situation == 2){
      nevers = neversdum * rnorm(n, -1.7, 1) 
      ss = neversdum * rnorm(n, -1.7, 1) 
      s = sdum * rnorm(n, 0.75,  1)
    }
    if(situation == 3){
      nevers = neversdum * rnorm(n, 0.75,  1) 
      ss = ssdum * rnorm(n, -1.7, 1)
      s = sdum * rnorm(n, 0.75,  1)
    }
    if(situation == 4){
      nevers = neversdum * rnorm(n, -1.7, 1) 
      ss = ssdum * rnorm(n, 0.75, 1)
      s = sdum * rnorm(n, 1.8,  1)
    }
      
    
    data$attitude <<- nevers + ss + s + rnorm(n, mean = 0.4, sd = 0.3)
  })
  
 
  regfunc <- function(x, int=0, bet1 = 0) {
    (int + bet1*(x-1))
  }
  
 
output$scatterplot <- renderPlot({
 
  df <- data.frame(attitude = data$attitude, smokestatus = data$smokestatus,
                   smokestatus2 = data$smokestatus2)

  mod1 <- lm(attitude ~ smokestatus, data = df) #ref cat = never smoked
  mod2 <- lm(attitude ~ smokestatus2, data = df) #ref cat = stopped smoking
  pval1 <- round(coef(summary(mod1))[2,4],4) #line never-stopped
  pval2 <- round(coef(summary(mod1))[3,4],4) #line never-smoking
  pval3 <- round(coef(summary(mod2))[3,4],4) #line stopped-smoking
  meaningroup = data.frame(x = c(1,2,3), means = as.vector(by(df$attitude,df$smokestatus,mean)),colour = names(by(df$attitude,df$smokestatus,mean)))
  
  ggplot(df, aes(x = smokestatus, y = attitude, colour = smokestatus)) + 
    geom_jitter(width = 0.1, show.legend = FALSE) +
    geom_segment(inherit.aes=FALSE,data=meaningroup,
                 aes(x = x-0.2,xend = x + 0.2,y=means,yend = means, colour = colour), 
                 size = 1.5, show.legend = FALSE) + 
    #non-smoker - former smoker line
    stat_function(inherit.aes = FALSE,data = data.frame(x = c(1,3)),aes(x=x), 
                  fun = regfunc, args = list(int = coef(mod1)[1],bet1 = coef(mod1)[2]),
                  colour = "grey",
                  alpha = ifelse(input$selector == "Smoker", 0, 1)) + 
    #non-smoker - smoker line
    stat_function(inherit.aes = FALSE,data = data.frame(x = c(1,3)),aes(x=x), 
                  fun = regfunc, args = list(int = coef(mod1)[1],bet1 = coef(mod1)[3]/2), 
                  colour = "black",
                  alpha = ifelse(input$selector == "Former smoker", 0, 1)) + 
    #smoker - former smoker line
    stat_function(inherit.aes = FALSE,data = data.frame(x = c(1,3)),aes(x=x), 
                  fun = regfunc, args = list(int = coef(mod2)[1]- coef(mod2)[3],bet1 = coef(mod2)[3]), 
                  colour = ifelse(input$selector == "Former smoker","black","grey"),
                  alpha = ifelse(input$selector == "Non-smoker", 0, 1)) + 
    #coefficient for non-smoker - former smoker line
    geom_text(inherit.aes=FALSE,x = 1.5,y=regfunc(1.5,int = coef(mod1)[1],bet1 = coef(mod1)[2]) + 0.8,
              label = paste0("b = ", rprint(coef(mod1)[2]), "\n", pprint(pval1)),
              colour = "grey", 
              alpha = ifelse(input$selector == "Smoker", 0, 1)) +
    #coefficient for non-smoker - smoker line
    geom_text(inherit.aes=FALSE,x = 2.0,y=regfunc(2.0,int = coef(mod1)[1],bet1 = coef(mod1)[3]/2) - 0.8,
              label = paste0("b = ", rprint(coef(mod1)[3]), "\n", pprint(pval2)),
              colour = "black", 
              alpha = ifelse(input$selector == "Former smoker", 0, 1)) +
    #coefficient for smoker - former smoker line
    geom_text(inherit.aes=FALSE,x = 2.5,y=regfunc(2.5,int = coef(mod2)[1]- coef(mod2)[3],bet1 = coef(mod2)[3]) + 0.8,
              label = paste0("b = ", rprint(coef(mod2)[3]), "\n", pprint(pval3)), 
              colour = ifelse(input$selector == "Former smoker","black","grey"),
              alpha = ifelse(input$selector == "Non-smoker", 0, 1)) +
    geom_text(inherit.aes=FALSE,data=meaningroup,aes(x = x + 0.4,y=means + 0.05, 
                                                     label = rprint(means),colour = colour),
              position = position_dodge(width=0.5), size = 5, show.legend = FALSE) + 
    xlab("Smoking status") + 
    coord_cartesian(ylim = c(-5, 5)) +
    ylab("Attitude") +
    theme_general()
})
  
})
    