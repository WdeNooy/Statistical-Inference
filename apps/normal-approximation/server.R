library(shiny)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  # Define server logic required to draw a histogram
  source("../plottheme/styling.R", local = TRUE)
  
  ##VARIABLES
  N <- 400 #sample size
  popmean <- 2.8 # mean of sample
  binwidth <- .1 # binwidth
  minpopsd <- 0.2 # minimal sd to sample
  maxpopsd <- 0.8 # maximal sd to sample
  #reactive values container holding sample and sd
  data <- reactiveValues(sample = as.numeric(),
                         sd = as.numeric()
                         )
  
  #Observe the sample button and generate a random sd
  #and generate a sample with new sd
  observeEvent(input$sample, {
    data$sd <<- runif(n = 1,
                      min = minpopsd,
                      max = maxpopsd)
    data$sample <<- rnorm(N,
                          mean =  2.8,
                          sd = data$sd
                          )
  })
  
  ## MAIN PLOT
  output$mainplot <- renderPlot({
    # validate(
    #   need(data$sample != "", "Please draw a sample")
    # )
    if (length(data$sample) == 0) {
      #Create an initial dataset
      data$sd <<- runif(n = 1,
                        min = minpopsd,
                        max = maxpopsd)
      data$sample <<- rnorm(N,
                            mean =  2.8,
                            sd = data$sd
      )
    }
    
    #Calculate the upper and lower 2.5% threshold
    lowerthres <-
      qnorm(
        p = 0.025,
        mean = popmean,
        sd = data$sd,
        lower.tail = TRUE
      )
    upperthres <-
      qnorm(
        p = 0.025,
        mean = 2.8,
        sd = data$sd,
        lower.tail = FALSE
      )
    
    # load sample from reactive container
    datahist <- data.frame(candyweight = data$sample)
    
    #Cut the data by threshold and store in variable
    datahist$colours <- cut(datahist$candyweight,
                             breaks = c(0,lowerthres,upperthres,6),
                            include.lowest = TRUE
                            )
    
    #determine break names
    brk <-levels(datahist$colours)
    
    #Variable for colors of histogram bars
    colors <- unname(c(brewercolors["Red"],
                       brewercolors["Blue"],
                       brewercolors["Green"]
                       )
                     )
    
    ##Main plot
    ggplot(datahist, aes(x = candyweight, fill = colours)) +
      #histogram layer
      geom_histogram(stat = "bin",
                     binwidth = binwidth) +
      #normal distribution layer
      stat_function(
        #function for scaling distribution to be visible in plot
        fun = function(x, mean, sd, n, bw) {
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        },
        args = c(
          mean = mean(datahist$candyweight),
          sd = sd(datahist$candyweight),
          n = N,
          bw = binwidth
        )
      ) +
      #upper threshold line
      geom_vline(
        aes(xintercept = upperthres,
            linetype = "Upper 2.5%")
      ) +
      #lower threshold line
      geom_vline(
        aes(xintercept = lowerthres,
            linetype = "Lower 2.5%")
      ) +
      #mean line
      geom_vline(
        aes(xintercept = 2.8,
            linetype = "Mean = 2.8")
      ) +
      #definition of cartesian coordinate
      coord_cartesian(xlim = c(0,6), ylim = NULL, expand = TRUE) + 
      #definition of break points on x axis
      scale_x_continuous(name = "Average candy weight per sample", breaks = 0:6) + 
      scale_y_continuous(name = "count") +
      #definition of linetypes for legend
      scale_linetype_manual("Legend:",values=c("dashed", "solid", "dashed"))+
      #definition for coloring of the bars
      scale_fill_manual("", breaks = brk, values = colors, drop = FALSE)+
      #surpress guides/legend for bars
      guides(fill = FALSE) +
      #apply general theme
      theme_general() + 
      #set legend to top of plot
      theme(legend.position = "top")
  })
})

