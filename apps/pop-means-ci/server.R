library(ggplot2)
library(shiny)

shinyServer(function(input, output) {
  source("../plottheme/styling.R", local = TRUE)
  
  #Limits of scales
  xmin <- 0 
  xmax <- 5
  ymin <- 0
  ymax <- 0.7
  
  #Size of point
  psize <- 5
  
  #Sampling distribution standard error
  se <- runif(1, 0.4, 0.6)
  
  #Draw sample mean and store with other unique values
  samplemean <- runif(1, 2, 4)
  sample_ll <- samplemean - 1.96 * se
  sample_ul <- samplemean + 1.96 * se
  smean <- data.frame(x = samplemean, #last selected populaiton mean
                      z = 0, #z value of sample mean for current pop. mean
                      xlow = xmin, xhigh = xmax,
                      ypop = ymin, #95%ci triangle
                      llim = samplemean, ulim = samplemean, #current ci limits
                      colour = "grey") #colour of triangle
    
  #Initialize data frames/objects for clicks
  df <- data.frame(x = xmin, y = ymin, colour = "grey") #First point (invisible)
  
  #Initialize interaction results
  result <- data.frame(ll_reached = "", ul_reached = "", both_reached = "", ntry = 0)

  ##MAIN PLOT##
  output$mainplot <- renderPlot({
    
    #Capture coordinates of click
    if (!is.null(input$plot_click$x)) {
      x <- input$plot_click$x
      #Adjust x values outside plotable area
      if (x < xmin) x <- xmin
      if (x > xmax) x <- xmax
      #Set x to critical value if near critical value
      x <- ifelse((samplemean - x)/se > 1.9 & (samplemean - x)/se <= 1.965, 
                  samplemean - 1.96 * se,
             ifelse((samplemean - x)/se < -1.9 & (samplemean - x)/se >= -1.965, 
                    samplemean + 1.96 * se, x))
      out <- abs(samplemean - x)/se > 1.965 #Outside 95%CI?
      df <<- rbind(df, data.frame(x = x, 
                                  y = ymax - psize/200,
                                  colour = ifelse(out, brewercolors["Red"], brewercolors["Green"])))
      #Update 95% confidence interval and z value of sample mean
      ll_cur <- smean$llim
      ul_cur <- smean$ulim
      smean <<- data.frame(x = x,  
                           z = round((samplemean - x)/se, digits = 2), # ifelse((samplemean - x)/se > 1.9 & (samplemean - x)/se <= 1.96, 
                                      # 1.96,
                                      # ifelse((samplemean - x)/se < -1.9 & (samplemean - x)/se >= -1.96, 
                                      #        -1.96, (samplemean - x)/se)), #around 1.96 -> 1.96
                           xlow = x - 1.96 * se, 
                           xhigh = x + 1.96 * se, 
                           ypop = ymax,
                           llim = ifelse(result$ll_reached == "", x, ll_cur),
                           ulim = ifelse(result$ul_reached == "", x, ul_cur),
                           colour = ifelse(out, brewercolors["Red"], brewercolors["Green"]))
      #Update results
      result$ntry <- result$ntry + 1
      if (smean$z == 1.96) result$ll_reached <- "Lower limit reached"
      if (smean$z == -1.96) result$ul_reached <- "Upper limit reached"
      if (result$ll_reached != "" & result$ul_reached != "") 
        result$both_reached <- paste0(result$ntry, " Clicks")
      result <<- result
    }
    
    #PLOT#
    ggplot() + 
      geom_blank() +
      #95% most likely sample means
      geom_segment(aes(x = (xhigh + xlow)/2, xend = xlow, 
                       y = ypop - psize/200, yend = ymin), data = smean,
                   alpha = ifelse(smean$ypop == ymin, 0, 1),
                   colour = smean$colour) +
      geom_segment(aes(x = (xhigh + xlow)/2, xend = xhigh, 
                       y = ypop - psize/200, yend = ymin), data = smean,
                   alpha = ifelse(smean$ypop == ymin, 0, 1),
                   colour = smean$colour) +
      geom_segment(aes(x = xlow, xend = xhigh, 
                       y = ymin, yend = ymin), data = smean,
                   alpha = ifelse(smean$ypop == ymin, 0, 1),
                   size = 3,
                   colour = smean$colour) +
      geom_text(aes(x = df$x[nrow(df)], y = (ymin + ymax)/4),
                label = "95% most likely samples",
                alpha = ifelse(smean$ypop == ymin, 0, 1),
                colour = smean$colour,
                size = 5) +
      #Critical value times standard error: arrows and text
      #left arrow with label
      geom_segment(aes(x = samplemean, xend = sample_ll, 
                       y = (ymax + ymin)/2, yend = (ymax + ymin)/2), 
                   alpha = ifelse(result$ll_reached == "", 0, 1),
                   size = 1,
                   colour = "darkgray",
                   arrow = arrow(type = "closed", length = unit(0.1, "inches"), 
                                 ends = ifelse(result$ll_reached == "" || result$ul_reached == "", "both", "last"))) +
      geom_text(aes(x = (samplemean + sample_ll)/2, y = (ymax + ymin)*0.47,
                    label = ifelse(result$ll_reached == "", "", "1.96 * SE"),
                    vjust = 1),
                colour = "darkgray") +
      #right arrow with label
      geom_segment(aes(x = samplemean, xend = sample_ul, 
                       y = (ymax + ymin)/2, yend = (ymax + ymin)/2), 
                   alpha = ifelse(result$ul_reached == "", 0, 1),
                   size = 1,
                   colour = "darkgray",
                   arrow = arrow(type = "closed", length = unit(0.1, "inches"), 
                                 ends = ifelse(result$ll_reached == "" || result$ul_reached == "", "both", "last"))) +
      geom_text(aes(x = (samplemean + sample_ul)/2, y = (ymax + ymin)*0.47,
                    label = ifelse(result$ul_reached == "", "", "1.96 * SE"),
                    vjust = 1),
                colour = "darkgray") +
      #confidence interval text: display if both limits have been reached
      geom_text(aes(x = samplemean, y = ymax*0.54,
                    label = ifelse(result$ll_reached == "" || result$ul_reached == "", "", "95% confidence interval"),
                    vjust = 0),
                colour = "darkgray") +
      #vertical line to actual sample mean (show if at least one limit has been reached)
      geom_segment(aes(x = samplemean, xend = samplemean, 
                       y = ymin, yend = (ymax + ymin)/2), 
                   alpha = ifelse(result$ll_reached == "" && result$ul_reached == "", 0, 1),
                   size = 1,
                   colour = "darkgray") +
      #vertical line to lower limit population value
      geom_segment(aes(x = sample_ll, xend = sample_ll, 
                       y = ymax, yend = (ymax + ymin)/2), 
                   alpha = ifelse(result$ll_reached == "", 0, 1),
                   size = 1,
                   colour = "darkgray") +
      #vertical line to upper limit population value
      geom_segment(aes(x = sample_ul, xend = sample_ul, 
                       y = ymax, yend = (ymax + ymin)/2), 
                   alpha = ifelse(result$ul_reached == "", 0, 1),
                   size = 1,
                   colour = "darkgray") +
      #Selected population means (on click)
      geom_point(aes(x = df$x, y = df$y), 
                 alpha = ifelse(df$y == ymin, 0, 1),
                 size = psize,
                 colour = df$colour) +
      geom_text(aes(x = smean$x, y = ymax - psize/80,
                    label = format(round(smean$x, digits = 2), nsmall=2),
                    vjust = 1),
                alpha = ifelse(smean$ypop == ymin || abs(smean$z) == 1.96, 0, 1)) +
      #Lower and upper bounds
      geom_text(aes(x = smean$llim, y = ymax - psize/80,
                    label = ifelse(result$ll_reached == "","", 
                                   paste0(format(round(smean$llim, digits = 2), nsmall=2), "\nLower limit")),
                    vjust = 1)) +
      geom_text(aes(x = smean$ulim, y = ymax - psize/80,
                    label = ifelse(result$ul_reached == "","", 
                                   paste0(format(round(smean$ulim, digits = 2), nsmall=2), "\nUpper limit")),
                    vjust = 1)) +
      #Sample mean
      geom_point(aes(x = samplemean, y = ymin + psize/200), 
                 size = psize) +
      geom_text(aes(x = samplemean + (xmax + xmin)*0.02, y = (ymax + ymin)*0.05, 
                    label = "Our Sample"),
                hjust = 0) +
      #z Value of sample mean
      geom_text(aes(x = samplemean + (xmax + xmin)*0.02, y = (ymax + ymin)*0.11, 
                    label = paste0("z = ", format(round(smean$z, digits = 2), nsmall=2))),
                hjust = 0,
                alpha = ifelse(smean$ypop == ymin, 0, 1)) +
      #Results
      geom_text(aes(x = xmin, y = (ymax + ymin)*0.7,
                    label = result$ll_reached[1],
                    hjust = 0),
                colour = brewercolors["Blue"]) +
      geom_text(aes(x = xmax, y = (ymax + ymin)*0.7,
                    label = result$ul_reached[1],
                    hjust = 1),
                colour = brewercolors["Blue"]) +
      geom_text(aes(x = (xmax + xmin)/2, y = (ymax + ymin)*0.7,
                    label = result$both_reached[1]),
                hjust = 0.5,
                colour = brewercolors["Blue"]) +
      #Scaling and double axis definitions
      scale_x_continuous(breaks = seq(xmin, xmax, by = 1), 
                         #limits = c(xmin, xmax),
                         sec.axis = sec_axis(~ .,
                           seq(xmin, xmax, by = 1),
                           name = "Average candy weight in the population"),
                         expand = c(.02, .02)) +
      scale_y_continuous(breaks = NULL, 
                         #limits = c(ymin, ymax),
                         expand = c(0, 0)) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      #Axis labels and theme                                       
      xlab("Average candy weight in the sample") + 
      ylab("") + 
      theme_general() +
      theme(panel.border = element_rect(colour = NA))
  })
  
})
