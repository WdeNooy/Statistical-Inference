library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 4 #sample size per category
  categories <- 3 #categories
  
  #Function to space dots in plot
  spacemaker <-  function(categories, samples, spacing) {
    x <- numeric()
    for (i in 1:categories) {
      temp <-
        seq(i - categories / 2 * spacing, i + categories / 2 * spacing, by = spacing)
      x <- c(x, temp)
    }
    return(x)
  }
  
  output$mainplot <- renderPlot({
    
    #Dependance on new sample button
    input$newsampbut
    isolate({
    mCloon <- runif(1,min = 3, max = 7)
    mJolie <- runif(1,min = 3, max = 7)
    mNoEnd <- runif(1,min = 3, max = 7)
    mMen <-  runif(1,min = -1, max = 1)
    mWom <- runif(1,min = -1, max = 1)
    })
    #Data frame containing samples of observations
    df <- isolate(
      data.frame(
        y = c(
          mCloon + rnorm(1,mMen,sd = 1),mCloon + rnorm(1,mMen,sd = 1), 
          mCloon + rnorm(1,mWom,sd = 1), mCloon + rnorm(1,mWom,sd = 1),
          mJolie + rnorm(1,mMen,sd = 1),mJolie + rnorm(1,mMen,sd = 1), 
          mJolie + rnorm(1,mWom,sd = 1), mJolie + rnorm(1,mWom,sd = 1),
          mNoEnd + rnorm(1,mMen,sd = 1),mNoEnd + rnorm(1,mMen,sd = 1),
          mNoEnd + rnorm(1,mWom,sd = 1), mNoEnd + rnorm(1,mWom,sd = 1)
        ),
        x = spacemaker(3,n,.15),
        cat1 = rep(
          c(
            "Clooney",
            "Jolie",
            "NoEnd"
          ),
          each = n
        ),
        cat2 = rep(
          c(
            "Men",
            "Women"
          ),
          each = 2
        )
      )
    )
    #Dataframe containing arrow positions 
    #tot = arrows between observation + grand mean
    #grpgr = grand mean to group mean
    #grpob = group mean to observation
    # dfarrows <- data.frame(x.tot = df$x,
    #                        xend.tot = df$x,
    #                        y.tot = rep(mean(df$y),length(df$y)),
    #                        yend.tot = df$y,
    #                        x.grpgr = df$x + 0.05,
    #                        xend.grpgr = df$x + 0.05,
    #                        y.grpgr = rep(mean(df$y), length(df$y)),
    #                        yend.grpgr = rep(as.vector(by(df$y, df$cat, mean, simplify = TRUE)),
    #                                         each = 4),
    #                        
    #                        x.grpob = df$x - 0.05,
    #                        xend.grpob = df$x - 0.05,
    #                        y.grpob = rep(as.vector(by(df$y, df$cat, mean, simplify = TRUE)),
    #                                      each = 4),
    #                        yend.grpob = df$y
    # )
    # 
    
    p <-
      ggplot(df, aes(x = x, y = y)) +
      #Grand mean line
      geom_line(y = mean(df$y),
                aes(linetype = "Grand Mean"),
                size = 1.3) +
      geom_point(size = 3,
                 aes(fill = cat1,
                     shape = cat2)) +
      #Manual color definitions
      scale_colour_manual(values = c(
        "Clooney" = unname(brewercolors["Orange"]),
        "Jolie" = unname(brewercolors["Blue"]),
        "No Endorser" = unname(brewercolors["Green"])
      )) +
      #Manual fill definitions
      scale_fill_manual(values = c(
        "Clooney" = unname(brewercolors["Orange"]),
        "Jolie" = unname(brewercolors["Blue"]),
        "NoEnd" = unname(brewercolors["Green"])
      )) +
      scale_shape_manual(values = c("Men" = 21,
                                    "Women" = 22)) +
      guides(
        colour = guide_legend(title = "Group Means"),
        linetype = guide_legend(title = "Grand Mean", label = FALSE),
        fill = FALSE
      ) +
      scale_x_continuous(breaks = 1:3,
                         labels = c("Clooney", "Jolie", "No Endorser")) +
      coord_cartesian(ylim = c(min(df$y) - 1, max(df$y) + 1)) +
      xlab("") +
      ylab("Willingness") +
      theme_general() +
      theme(legend.position = "top")
    
    inpval <- isolate(input$groupselect)
   
    if("1" %in% inpval & length(inpval) == 1){
      print("test")
      p <-
        p + 
          #Mean of group Clooney
          geom_segment(x = min(subset(df, cat1 == "Clooney")$x - .09),
                       xend = max(subset(df, cat1 == "Clooney")$x + .09),
                       y = mean(subset(df, cat1 == "Clooney")$y),
                       yend = mean(subset(df, cat1 == "Clooney")$y),
                       size = 1.2,
                       aes(colour = "Clooney"))
      #   #Mean of Group Jolie
      #   geom_segment(x = min(subset(df, cat == "Jolie")$x - .09),
      #                xend = max(subset(df, cat == "Jolie")$x + .09),
      #                y = mean(subset(df, cat == "Jolie")$y),
      #                yend = mean(subset(df, cat == "Jolie")$y),
      #                size = 1.2,
      #                aes(colour = "Jolie")) +
      #   #Mean of Group No Endorser
      #   geom_segment(x = min(subset(df, cat == "NoEnd")$x - .09),
      #                xend = max(subset(df, cat == "NoEnd")$x + .09),
      #                y = mean(subset(df, cat == "NoEnd")$y),
      #                yend = mean(subset(df, cat == "NoEnd")$y),
      #                size = 1.2,
      #                aes(colour = "No Endorser")) +
      #   Arrows from grand mean to observation
      # geom_segment(x = dfarrows$x.tot,
      #              xend = dfarrows$xend.tot,
      #              y = dfarrows$y.tot,
      #              yend = dfarrows$yend.tot,
      #              linetype = "dotted",
      #              arrow = arrow(length = unit(2,"mm"),
      #                            ends = "both",
      #                            type = "closed")
      # ) +
      #   #Arrows from grand mean to group mean
      #   geom_segment(x = dfarrows$x.grpgr,
      #                xend = dfarrows$xend.grpgr,
      #                y = dfarrows$y.grpgr,
      #                yend = dfarrows$yend.grpgr,
      #                linetype = "solid",
      #                arrow = arrow(length = unit(2,"mm"),
      #                              ends = "both",
      #                              type = "closed")) +
      #   #Arrows from group to observation
      #   geom_segment(x = dfarrows$x.grpob,
      #                xend = dfarrows$xend.grpob,
      #                y = dfarrows$y.grpob,
      #                yend = dfarrows$yend.grpob,
      #                linetype = "solid",
      #                color = "red",
      #                arrow = arrow(length = unit(2,"mm"),
      #                              ends = "both",
      #                              type = "closed")) +
      p
    }
    p
      #Points representing observations
 
    
  })
  
  
})
