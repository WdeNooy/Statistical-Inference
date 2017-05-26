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
  smean <- data.frame(x = samplemean, y = ymin + psize/160, 
                      z = 0, #z value of sample mean for current pop. mean
                      xlow = xmin, xhigh = xmax,
                      ypop = ymin, #95%ci triangle
                      ylab = ymin, #label location
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
      out <- abs(samplemean - x)/se > 1.96 #Outside 95%CI?
      df <<- rbind(df, data.frame(x = x, 
                                  y = ymax - psize/200,
                                  colour = ifelse(out, brewercolors["Red"], brewercolors["Green"])))
      #Update 95% confidence interval and z value of sample mean
      ll_cur <- smean$llim[1]
      ul_cur <- smean$ulim[1]
      smean <<- data.frame(x = samplemean, y = ymin + psize/200, 
                           z = (samplemean - x)/se,
                           xlow = x - 1.96 * se, 
                           xhigh = x + 1.96 * se, 
                           ypop = ymax,
                           ylab = ymin + 4*psize/200,
                           llim = ifelse(!out & x < ll_cur, x, ll_cur),
                           ulim = ifelse(!out & x > ul_cur, x, ul_cur),
                           colour = ifelse(out, brewercolors["Red"], brewercolors["Green"]))
      #Update results
      result$ntry <- result$ntry + 1
      if (smean$z > 1.9 & smean$z <= 1.96) result$ll_reached <- "Lower limit reached"
      if (smean$z < -1.9 & smean$z >= -1.96) result$ul_reached <- "Upper limit reached"
      if (result$ll_reached != "" & result$ul_reached != "") 
        result$both_reached <- paste0(result$ntry, " Clicks")
      result <<- result
    }
    
    #PLOT#
    ggplot() + 
      geom_blank() +
      #Sample mean
      geom_point(data = smean, aes(x = x, y = y), 
                 size = psize) +
      #z Value of sample mean
      geom_text(aes(x = smean$x, y = smean$ylab, 
                     label = paste0("z = ", round(smean$z, digits = 2))),
                    alpha = ifelse(smean$ypop == ymin, 0, 1)) +
      #95% most likely sample means
      geom_segment(aes(x = (xhigh + xlow)/2, xend = xlow, 
                       y = ypop - psize/100, yend = ypop/2), data = smean,
                   alpha = ifelse(smean$ypop == ymin, 0, 1)) +
      geom_segment(aes(x = (xhigh + xlow)/2, xend = xhigh, 
                       y = ypop - psize/100, yend = ypop/2), data = smean,
                   alpha = ifelse(smean$ypop == ymin, 0, 1)) +
      geom_segment(aes(x = xlow, xend = xhigh, 
                       y = ypop/2, yend = ypop/2), data = smean,
                   alpha = ifelse(smean$ypop == ymin, 0, 1),
                   size = 3,
                   colour = smean$colour) +
      geom_text(aes(x = df$x[nrow(df)], y = (ymax - 0.1)/2),
                label = "95% most likely samples",
                alpha = ifelse(smean$ypop == ymin, 0, 1)) +
      #Lower and upper bounds
      geom_text(aes(x = smean$llim, y = ymax - psize/40,
                    label = paste0("Lower limit\n", round(smean$llim, digits = 2))),
                alpha = ifelse(smean$llim == samplemean, 0, 1)) +
      geom_text(aes(x = smean$ulim, y = ymax - psize/40,
                    label = paste0("Upper limit\n", round(smean$ulim, digits = 2))),
                alpha = ifelse(smean$ulim == samplemean, 0, 1)) +
      #Population means (on click)
      geom_point(aes(x = df$x, y = df$y), 
                 alpha = ifelse(df$y == ymin, 0, 1),
                 size = psize,
                 colour = df$colour) +
      #Results
      geom_text(aes(x = xmin + (xmax - xmin)/6, y = ymin + 7*psize/200,
                    label = result$ll_reached[1]),
                colour = brewercolors["Blue"]) +
      geom_text(aes(x = xmax - (xmax - xmin)/6, y = ymin + 7*psize/200,
                    label = result$ul_reached[1]),
                colour = brewercolors["Blue"]) +
      geom_text(aes(x = (xmax + xmin)/2, y = ymin + 7*psize/200,
                    label = result$both_reached[1]),
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
