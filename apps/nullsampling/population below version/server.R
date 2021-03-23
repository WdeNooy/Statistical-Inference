library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)
  
  se <- 0.7  #sd of sampling distribution
  df <- 30 #df of t distribution
  sampsd <- 1.0 #sd of sample
  tc <- qt(0.025, df, lower.tail = FALSE) #criticial quantiles

  #Function for scaling and shifting the t-distribution
  dtshift <- function(x,mean,sd,df) 0.8*dt(x = (x - mean)/sd, df = df)
  
  #Reactive containers for changing values
  reactive <- 
    reactiveValues(
      sampmean = ifelse(exists("react$sample"), runif(1, 3, 8), 3.9) #initial saple mean = 3.9
    )
  react <- 
    reactiveValues(
      sample = data.frame(x = rnorm(df + 1, mean = isolate(reactive$sampmean), sd = sampsd), y = 0.35) #sample
    )
  
  #Reset
  observeEvent(input$resetButton, {
    reactive$sampmean <- runif(1, 3, 8) #sample mean
    react$sample <- data.frame(x = rnorm(df + 1, mean = reactive$sampmean, sd = sampsd), y = 0.35) #sample
  })
  
  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    mean <- input$sampsizeslider #hypothesized population mean
    
    #Calculating the left threshold
    left <- mean - se * tc
    right <- mean + se * tc

    #Calculates two-tailed p-value
    pvalue <- 2*pt(
      q = -abs((reactive$sampmean - input$sampsizeslider) / se),
      df = df
      )

    #PLOT#
    ggplot(data.frame(x = c(0,8)), aes(x = x)) + 
      #Left area under curve: 2.5%
      stat_function(fun = dtshift,
                    xlim = c(1,left),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = ifelse(
                      input$steps == "step1", #in the first step...
                      "white", #...don't show...
                      "black" #...else, show
                    ),
                    alpha = ifelse(
                      input$steps == "step1", #in the first step...
                      0, #...don't show...
                      1 #...else, show
                    ),
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #Right area under curve: 2.5%
      stat_function(fun = dtshift,
                    xlim = c(right, 10),
                    geom = "area",
                    fill = brewercolors["Blue"],
                    colour = ifelse(
                      input$steps == "step1", #in the first step...
                      "white", #...don't show...
                      "black" #...else, show
                    ),
                    alpha = ifelse(
                      input$steps == "step1", #in the first step...
                      0, #...don't show...
                      1 #...else, show
                    ),
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #T distribution function
      stat_function(fun = dtshift,
                    args = list(mean = mean, sd = se, df = df),
                    n = 1000) +
      #2.5% label left
      geom_text(label = "2.5%",
                aes(x = left - 0.3,
                    y =  dtshift(left, mean, se, df) - 0.01),
                alpha = ifelse(
                  input$steps == "step1", #in the first step...
                  0, #...don't show...
                  1 #...else, show
                ),
                hjust = 1,
                size = 5) +
      #2.5% label right
      geom_text(label = "2.5%",
                aes(x = right + 0.3,
                    y =  dtshift(right, mean, se, df) - 0.01),
                alpha = ifelse(
                  input$steps == "step1", #in the first step...
                  0, #...don't show...
                  1 #...else, show
                ),
                hjust = 0,
                size = 5) +
      # #Horizontal axis for sampling distribution
      # geom_hline(aes(yintercept = 0)) +
      #Hypothesized population mean line
      geom_segment(aes(x = mean, xend = mean,
                       y = 0, yend = dtshift(mean, mean, se, df))) +
      #sample scores
      geom_point(data = react$sample[react$sample$x >= 1 & react$sample$x <= 10,], 
                 aes(x = x, y = y), 
                 colour = brewercolors["Red"],
                 alpha = ifelse(
                   input$steps %in% c("step3", "step4"), #after the second step...
                   1, #...show...
                   0 #...else, don't show
                 )
      ) +
      #Sample average vline
      geom_segment(aes(x = reactive$sampmean, xend = reactive$sampmean, 
                       y = 0, yend = 0.4), 
                   colour = brewercolors["Red"],
                   alpha = ifelse(
                     input$steps %in% c("step3", "step4"), #after the second step...
                     1, #...show...
                     0 #...else, don't show
                   )
                   ) +
      #Rejection region
      #left part
      geom_segment(
        aes(x = 1, xend = left, y = 0, yend = 0),
        size = 1.5,
        color = brewercolors["Blue"],
        alpha = ifelse(
          input$steps == "step1", #in the first step...
          0, #...don't show...
          1 #...else, show
        )
      ) +
      #right part
      geom_segment(
        aes(x = 10, xend = right, y = 0, yend = 0),
        size = 1.5,
        color = brewercolors["Blue"],
        alpha = ifelse(
          input$steps == "step1", #in the first step...
          0, #...don't show...
          1 #...else, show
        )
      ) +
      #Decision
      geom_text(
        aes(x = 10, y = 0.25,
            label = paste0(
              "The sample mean is \n",
              ifelse(pvalue < 0.05,
                     "",
                     "not "
              ),
              "in the rejection region, \n",
              ifelse(
                pvalue < 0.05,
                "so reject the null hypothesis.",
                "so do not reject the null hypothesis."
              )
            )
        ),
        hjust = 1, vjust = 0.5,
        color = brewercolors["Blue"],
        alpha = ifelse(
          input$steps == "step4", #in the fourth step...
          1, #...show...
          0 #...else, don't show
        ),
        size = 5
      ) +
      # #Two-sided p value for current sample mean in top-right corner
      # geom_text(
      #   aes(x = 10, y = 0.25,
      #       label = paste0(
      #         "p value ",
      #         ifelse(pvalue < 0.001,
      #                "< 0.001",
      #                paste0("= ", format(round(pvalue, digits = 3), nsmall = 3))
      #           ),
      #         "\n",
      #         ifelse(
      #           pvalue < 0.05,
      #           "p < 0.05, so reject H0",
      #           "p > 0.05, so do not reject H0"
      #         )
      #       )
      #       ),
      #   hjust = 1, vjust = 0.5,
      #   alpha = ifelse(
      #     input$steps == "step4", #in the fourth step...
      #     1, #...show...
      #     0 #...else, don't show
      #   )
      # ) +
      #Scaling and double axis definitions
      scale_x_continuous(breaks = if(input$steps == "step1") {
                                    c(1, mean, 10)} else {
                                      c(1, left, mean, right, 10)}, 
                         limits = c(1, 10),
                         labels = if(input$steps == "step1") {
                                    c("1", format(round(mean, 2),nsmall = 2), "10")} else {
                                      c("1", format(round(left, 2),nsmall = 2),
                                        format(round(mean, 2),nsmall = 2),
                                        format(round(right, 2),nsmall = 2), "10")},
                         sec.axis = sec_axis(~ .,
                           breaks = if(input$steps %in% c("step1", "step2")) {
                             c(1, 10)} else {
                               c(1, reactive$sampmean, 10)},
                           labels = if(input$steps %in% c("step1", "step2")) {
                             c(1, 10)} else {
                               c(1, paste0("Mean = ",round(reactive$sampmean, digits = 2)), 10)},
                           name = "Sample media literacy scores"),
                         expand = c(.02, .02)) +
      scale_y_continuous(breaks = NULL, 
                         limits = c(0, 0.4),
                         expand = c(0, 0)) + 
      #Axis labels and theme                                       
      xlab("Hypothesized population mean media literacy score") + 
      ylab("") + 
      theme_general() +
      theme(panel.border = element_rect(colour = NA), 
            plot.margin = margin(0,0,0,0))
  })
})
