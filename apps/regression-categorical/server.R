library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  
  #Load styling for plots
  source("../plottheme/styling.R", local = TRUE)
  
  #CREATE PREDICTOR
  n <- 600
  
  data <- reactiveValues(
    smokestatus = numeric(),
    attitude = numeric(0)
  )
  
  observeEvent(input$samplebutton,ignoreNULL = FALSE,{
    situation <-sample(1:4,1)
    
    data$smokestatus <- as.factor(sample(c("Never smoked","Stopped smoking", "Smoking"),size = n, replace = TRUE))
    
    neversdum <- ifelse(data$smokestatus == "Never smoked", 1, 0) 
    ssdum <- ifelse(data$smokestatus == "Stopped smoking", 1, 0)
    sdum <- ifelse(data$smokestatus == "Smoking", 1,0)
    if(situation == 1){
      nevers = neversdum * rnorm(n, -1.7, 2) 
      ss = ssdum * rnorm(n, 0.75, 1)
      s = sdum * rnorm(n, 0.75,  1)
    }
    if(situation == 2){
      nevers = neversdum * rnorm(n, -1.7, 2) 
      ss = neversdum * rnorm(n, -1.7, 2) 
      s = sdum * rnorm(n, 0.75,  1)
    }
    if(situation == 3){
      nevers = neversdum * rnorm(n, 0.75,  1) 
      ss = ssdum * rnorm(n, -1.7, 2)
      s = sdum * rnorm(n, 0.75,  1)
    }
    if(situation == 4){
      nevers = neversdum * rnorm(n, -1.7, 2) 
      ss = ssdum * rnorm(n, 0.75, 1)
      s = sdum * rnorm(n, 1.8,  1)
    }
      
    
    data$attitude <<- nevers + ss + s + rnorm(n, mean = 0.4, sd = 0.9)
  })
  
 
  regfunc <- function(x, int=0, bet1 = 0) {
    (int + bet1*(x-1))
  }
  
 
output$scatterplot <- renderPlot({
 
  data$smokestatus <- relevel(as.factor(data$smokestatus), ref = input$selector)
  
  if(input$selector == "Smoking"){
    coloursmokestatus <- as.factor(ifelse(data$smokestatus != input$selector, "Never smoked/Stopped smoking", input$selector))
  }
  if(input$selector == "Never smoked"){
    coloursmokestatus <- as.factor(ifelse(data$smokestatus != input$selector, "Smoking/Stopped smoking", input$selector))
  }
  if(input$selector == "Stopped smoking"){
    coloursmokestatus <- as.factor(ifelse(data$smokestatus != input$selector, "Never smoked/Smoking", input$selector))
  }
  
  coloursmokestatus <- relevel(coloursmokestatus, ref = input$selector)
  #print(coloursmokestatus)
  
  df <- data.frame(attitude = data$attitude,smokestatus = data$smokestatus, coloursmokestatus)
  
  mod1 <- lm(attitude ~ smokestatus, data = df)
  pval1 <- round(coef(summary(mod1))[2,4],4)
  pval2 <- round(coef(summary(mod1))[3,4],4)
  meaningroup = data.frame(x = c(1,2,2), means = as.vector(by(df$attitude,df$smokestatus,mean)),colour = names(by(df$attitude,df$smokestatus,mean)))
  
  
  ggplot(df, aes(x = coloursmokestatus, y = attitude, colour = smokestatus)) + 
    geom_jitter(width = 0.1) +
    geom_segment(inherit.aes=FALSE,data=meaningroup,aes(x = x-0.2,xend = x + 0.2 ,y=means,yend = means),size = 1.1) + 
    stat_function(inherit.aes = FALSE,data = data.frame(x = c(1,3)),aes(x=x), fun = regfunc, args = list(int = coef(mod1)[1],bet1 = coef(mod1)[2])) + 
    stat_function(inherit.aes = FALSE,data = data.frame(x = c(1,3)),aes(x=x), fun = regfunc, args = list(int = coef(mod1)[1],bet1 = coef(mod1)[3])) + 
    geom_text(inherit.aes=FALSE,x = 2.8,y=regfunc(2.5,int = coef(mod1)[1],bet1 = coef(mod1)[2]) + 0.7,label = paste0("p <= ",pval1)) +
    geom_text(inherit.aes=FALSE,x = 2.8,y=regfunc(2.5,int = coef(mod1)[1],bet1 = coef(mod1)[3]) + 0.7,label = paste0("p <= ",pval2)) +
    geom_text(inherit.aes=FALSE,data=meaningroup,aes(x = x + 0.4,y=means + 0.5, label = round(means,2),colour = colour),position = position_dodge(width=0.5),size =5) + 
    xlab("Group") + 
    ylab("Attitude") +
    theme_general() + 
    theme(legend.position = "bottom")
})
  
})
    