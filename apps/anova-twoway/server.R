library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 4 # Sample size per category.
  categories <- 3 # no. of categories.
  
  # Function needed to space dots nicely in the plot
  
  spacemaker <-  function(categories, samples, spacing) {
    x <- numeric()
    for (i in 1:categories) {
      temp <-
        seq(i - categories / 2 * spacing, i + categories / 2 * spacing, by = spacing)
      x <- c(x, temp)
    }
    return(x)
  }
  
  samples <-
    reactive({
      # Samples is dependent on the new sample button.
      
      if ( input$newsampbut == 0 ) {
        #start data set with larger differences for endorser than for sex
        df <- data.frame(
          y = c(
            6 + rnorm(1, -0.4, sd = 0.6),
            6 + rnorm(1, -0.4, sd = 0.6),
            6 + rnorm(1, 0.4, sd = 0.6),
            6 + rnorm(1, 0.4, sd = 0.6),
            7 + rnorm(1, -0.4, sd = 0.6),
            7 + rnorm(1, -0.4, sd = 0.6),
            7 + rnorm(1, 0.4, sd = 0.6),
            7 + rnorm(1, 0.4, sd = 0.6),
            3 + rnorm(1, -0.4, sd = 0.6),
            3 + rnorm(1, -0.4, sd = 0.6),
            3 + rnorm(1, 0.4, sd = 0.6),
            3 + rnorm(1, 0.4, sd = 0.6)),
          x = spacemaker(3, n, .15),
          cat1 = rep(c("Clooney","Jolie","No Endorser"),each = n),
          cat2 = rep(c("Men","Women"),each = 2))
      }
      
      else {
        
        # Define the means for each category.
        
        mCloon <- runif(1, min = 3, max = 7)
        mJolie <- runif(1, min = 3, max = 7)
        mNoEnd <- runif(1, min = 3, max = 7)
        mMen <-   runif(1, min = -1, max = 1)
        mWom <-   runif(1, min = -1, max = 1)
        
        # Generate the data frame needed for plotting the positions of each dot
        # variable y is the mean of each endorser category mixed with a random 
        # bit for the sex category with sd of 1. 
        
        df <-
          data.frame(
            y = c(
              mCloon + rnorm(1, mMen, sd = 0.6),
              mCloon + rnorm(1, mMen, sd = 0.6),
              mCloon + rnorm(1, mWom, sd = 0.6),
              mCloon + rnorm(1, mWom, sd = 0.6),
              mJolie + rnorm(1, mMen, sd = 0.6),
              mJolie + rnorm(1, mMen, sd = 0.6),
              mJolie + rnorm(1, mWom, sd = 0.6),
              mJolie + rnorm(1, mWom, sd = 0.6),
              mNoEnd + rnorm(1, mMen, sd = 0.6),
              mNoEnd + rnorm(1, mMen, sd = 0.6),
              mNoEnd + rnorm(1, mWom, sd = 0.6),
              mNoEnd + rnorm(1, mWom, sd = 0.6)),
            x = spacemaker(3, n, .15),
            cat1 = rep(c("Clooney","Jolie","No Endorser"),each = n),
            cat2 = rep(c("Men","Women"),each = 2))
      }  
      return(df)
    })
  
  # MAIN PLOT ----
  
  output$mainplot <- renderPlot({
    
    df <- samples()
    
    # Plot With No Selection ----
    p <-
      ggplot(df, aes(x = x, y = y)) +
      
      geom_line(y = mean(df$y),
                aes(linetype = "Grand Mean", colour = "black"),
                size = 1.3) +
      geom_point(size = 5, aes(fill = cat1, shape = cat2)) +
      scale_colour_manual(values = 
                            c("Clooney" = unname(brewercolors["Orange"]),
                              "Jolie" = unname(brewercolors["Blue"]),
                              "No Endorser" = unname(brewercolors["Green"]))) +
      scale_fill_manual(values = 
                          c("Clooney" = unname(brewercolors["Orange"]),
                            "Jolie" = unname(brewercolors["Blue"]),
                            "No Endorser" = unname(brewercolors["Green"]))) +
      scale_shape_manual(values = c("Men" = 21, "Women" = 22)) +
      guides(colour = "none", # guide_legend(title = "Means:"),
             linetype = guide_legend(title = "Means:"),
             fill = "none",
             shape = guide_legend(title = "Sex:")) +
      scale_x_continuous(breaks = 1:3,
                         labels = c("Clooney", "Jolie", "No Endorser")) +
      coord_cartesian(ylim = c(min(df$y) - 1, max(df$y) + 1)) +
      xlab("") +
      ylab("Willingness") +
      theme_general() +
      theme(legend.position = "top")
    
    # Plot With only Endorser ----
    
    if ("1" %in% input$groupselect & length(input$groupselect) == 1)
    {
      
      # Create data frame for arrows between grand mean and group means.
      
      dfarrows <- data.frame(
        x.grpgr    = df$x ,
        xend.grpgr = df$x,
        y.grpgr    = rep(mean(df$y), length(df$y)),
        yend.grpgr = 
          rep(as.vector(by(df$y, df$cat1, mean, simplify = TRUE)), each =  4))
      
      p <-
        p +
        geom_segment( x    = min(subset(df, cat1 == "Clooney")$x - .09),
                      xend = max(subset(df, cat1 == "Clooney")$x + .09),
                      y    = mean(subset(df, cat1 == "Clooney")$y),
                      yend = mean(subset(df, cat1 == "Clooney")$y),
                      size = 1.2,
                      aes(colour = "Clooney",linetype = "Grand Mean")) +
        geom_segment( x    = min(subset(df, cat1 == "Jolie")$x - .09),
                      xend = max(subset(df, cat1 == "Jolie")$x + .09),
                      y    = mean(subset(df, cat1 == "Jolie")$y),
                      yend = mean(subset(df, cat1 == "Jolie")$y),
                      size = 1.2,
                      aes(colour = "Jolie",linetype = "Grand Mean")) +
        geom_segment( x    = min(subset(df, cat1 == "No Endorser")$x - .09),
                      xend = max(subset(df, cat1 == "No Endorser")$x + .09),
                      y    = mean(subset(df, cat1 == "No Endorser")$y),
                      yend = mean(subset(df, cat1 == "No Endorser")$y),
                      size = 1.2,
                      aes(colour = "No Endorser",linetype = "Grand Mean"))  +
        geom_segment( x        = dfarrows$x.grpgr,
                      xend     = dfarrows$xend.grpgr,
                      y        = dfarrows$y.grpgr,
                      yend     = dfarrows$yend.grpgr,
                      linetype = "solid",
                      colour   = "grey",
                      arrow    = 
                        arrow(length = unit(2, "mm"),
                              ends   = "both",
                              type   = "closed")) + 
        guides(colour = guide_legend(title = ""),
               linetype = guide_legend(title = "Means:"),
               fill = "none",
               shape = "none") + # guide_legend(title = "Sex:")) # +
        scale_linetype_manual( values = c("Grand Mean"  = "solid")) #,
                                          # "Clooney"     = "solid",
                                          # "Jolie"       = "solid",
                                          # "No Endorser" = "solid")) 
    }
    
    #Plot With Only Sex Selected ----
    
    if("2" %in% input$groupselect & length(input$groupselect) == 1)
    {
      
      # Data frame for arrows between grand and group means.
      
      dfarrows <-
        data.frame( x.grpgr    = df$x ,
                    xend.grpgr = df$x,
                    y.grpgr    = rep(mean(df$y), length(df$y)),
                    yend.grpgr = 
                      rep(as.vector(by(df$y, df$cat2, mean, simplify = TRUE)),3, each = 2)
        )
      
      p <-
        p +
        geom_segment( x    = min(df$x),
                      xend = max(df$x),
                      y    = mean(subset(df, cat2 == "Men")$y),
                      yend = mean(subset(df, cat2 == "Men")$y),
                      aes(linetype = "Men")) +
        geom_segment( x    = min(df$x),
                      xend = max(df$x),
                      y    = mean(subset(df, cat2 == "Women")$y),
                      yend = mean(subset(df, cat2 == "Women")$y),
                      aes(linetype = "Women")) +
        scale_linetype_manual(values = c("Grand Mean" = "solid",
                                         "Men"        = "dotted",
                                         "Women"      = "dashed")) +
        guides( colour       = "none",
                linetype     = guide_legend(title = "Means:",
                                            order = 2,
                                            override.aes = list(
                                              linetype = 
                                                c("solid", "dotted", "dashed"),
                                              colour = 
                                                c("black", "black", "black"),
                                              size = 0.5)),
                fill         = "none",
                shape        = guide_legend(title = "Sex:", order = 1)) +
        geom_segment( x        = dfarrows$x.grpgr,
                      xend     = dfarrows$xend.grpgr,
                      y        = dfarrows$y.grpgr,
                      yend     = dfarrows$yend.grpgr,
                      linetype = "solid",
                      colour   = "grey",
                      arrow    = arrow( length = unit(2, "mm"),
                                        ends   = "both",
                                        type   = "closed"))
    }
    
    # Plot With Endorser And Sex Selected ----
    
    if(length(input$groupselect) == 2)
    {
      
      meandf <-
        data.frame(
          x    = as.vector(by(df$x,INDICES = list(df$cat1,df$cat2),min) - 0.1), 
          xend = as.vector(by(df$x,INDICES = list(df$cat1,df$cat2),max) + 0.1),
          y    = as.vector(by(df$y,INDICES = list(df$cat1,df$cat2),mean)),
          cat1 = unique(df$cat1),
          cat2 = rep(unique(df$cat2),1,each = 3))
      
      meandf <- meandf[order(meandf$cat1,meandf$cat2),]
      
      dfarrows <-
        data.frame(x.grpgr    = df$x ,
                   xend.grpgr = df$x,
                   y.grpgr    = rep(mean(df$y), length(df$y)),
                   yend.grpgr = rep(meandf$y, 1, each = 2)) 
      
      p <-
        p +
        geom_segment(data = meandf,
                     aes(x        = x,
                         xend     = xend,
                         y        = y,
                         yend     = y,
                         colour   = cat1,
                         linetype =  cat2),
                     size = 1) +
        scale_linetype_manual(values = c("Grand Mean" = "solid",
                                         "Men" = "dotted",
                                         "Women" = "dashed")) +
        guides(colour = guide_legend(title = "Means:"),
               linetype = guide_legend(title = ""),
               fill = "none",
               shape = "none") + # guide_legend(title = "Sex:")) +
        geom_segment(x = dfarrows$x.grpgr,
                     xend = dfarrows$xend.grpgr,
                     y = dfarrows$y.grpgr,
                     yend = dfarrows$yend.grpgr,
                     linetype = "solid",
                     colour   = "grey",
                     arrow = arrow(
                       length = unit(2, "mm"),
                       ends = "both",
                       type = "closed"))
    }
    
    # Print the plot 
    p
    
  })
})