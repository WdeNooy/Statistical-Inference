library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  #Load styling files for plots
  source("../plottheme/styling.R", local = TRUE)

  n.sim <- 1000
  
  draw.sample <- reactive({
    
    m <- input$effect.size
    s <- 1
    sample.data <- rnorm(n.sim * input$sample.size, m, s)
    sample.data <- matrix(sample.data, nrow=n.sim)
    
    sample.means <- apply(sample.data, 1, mean)
    sample.sems <- apply(sample.data, 1, sd) / sqrt(input$sample.size)
    
    df <- input$sample.size - 1
    t.stats <- sample.means / sample.sems   
    p.values <- pt(t.stats, df, lower.tail=FALSE)
    
    sig.rate <- sum(p.values < 0.05) / n.sim
    power <- ifelse(m == 0, NA, power.t.test(input$sample.size, m, s, 0.05,
                          type="one.sample", alternative="one.sided")$power)
    
    list(df=df, t.stats=t.stats, p.values=p.values, sig.rate=sig.rate, power=power)
    
  })
    
  output$t.stats <- renderPlot({
    
    sample <- draw.sample()
    df_t <- data.frame(x = sample$t.stats)
    plot.title <- sprintf("Power: %.2f; Proportion rejected nulls: %.2f", sample$power, sample$sig.rate)
    ggplot(df_t, aes(x = x)) +
      geom_histogram(bins = 25, color = "black", fill = brewercolors[["Blue"]]) +
      theme_general() +
      labs(title = plot.title, x = "t-values of 1,000 samples", y = "Frequency") +
      geom_vline(xintercept = qt(0.05, sample$df, lower.tail=FALSE), size = 2)
    
  })
  
  output$p.values <- renderPlot({
    
    sample <- draw.sample()  
    df_p <- data.frame(x = sample$p.values)
    ggplot(df_p, aes(x = x)) +
      geom_histogram(bins = 40, color = "black", fill = brewercolors[["Red"]]) +
      theme_general() +
      labs(x = "p-values of 1,000 samples", y = "Frequency") +
      geom_vline(xintercept = 0.05, size = 2)
    
  })
  
})