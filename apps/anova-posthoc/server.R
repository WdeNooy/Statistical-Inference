library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 4 #sample size per category
  start = 0.5 #minimum value of x
  end = 4 #maximum value of x
  
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
    
    #Data frame containing samples of observations
    df <- isolate(
      data.frame(
        y = c(
          input$clooneynumin - 0.7, input$clooneynumin + 1.3, 
          input$clooneynumin - 1.1, input$clooneynumin + 0.5,
          input$jolienumin + 0.8, input$jolienumin + 0.6, 
          input$jolienumin - 1.2, input$jolienumin - 0.2,
          input$endorsernumin - 0.9, input$endorsernumin + 2.1, 
          input$endorsernumin - 0.5, input$endorsernumin - 0.7
        ),
        x = spacemaker(3,n,.15),
        cat = rep(
          c(
            "Clooney",
            "Jolie",
            "NoEnd"
          ),
          each = n
        )
      )
    )
    
    etasqrd  <- summary.lm(aov(data = df, y  ~ cat))$r.squared
    Fstat    <- unlist(summary(aov(data = df, y  ~ cat)))["F value1"]
    df1And2  <- unlist(summary(aov(data = df, y  ~ cat)))[1:2]
    pValue   <- unlist(summary(aov(data = df, y  ~ cat)))["Pr(>F)1"]
    
    postHocpValues <- pairwise.t.test(df$y,df$cat,p.adjust.method = "none")$p.value
    
    phJolCloon   <-  round(postHocpValues[1],2)
    phNoEndCloon <-  round(postHocpValues[2],2)
    phNoEndJol   <-  round(postHocpValues[4],2)
    
    ggplot(df,aes(x = x, y = y)) +
      #Mean of group Clooney
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "Clooney")$y),
                   size = 1.2,
                   aes(colour = "Clooney")) + 
      #Mean of Group Jolie
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Jolie")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   size = 1.2,
                   aes(colour = "Jolie")) + 
      #Mean of Group No Endorser
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "NoEnd")$y),
                   yend = mean(subset(df, cat == "NoEnd")$y),
                   size = 1.2,
                   aes(colour = "No Endorser")) +  
      #Arrows between group means
      geom_segment(x = end - 1.05,
                   xend = end - 1.05,
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      geom_segment(x = end - 0.55,
                   xend = end - 0.55,
                   y = mean(subset(df, cat == "NoEnd")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      geom_segment(x = end-0.1,
                   xend = end - 0.1,
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "NoEnd")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      #Points representing observations
      geom_point(shape = 21,
                 size = 5,
                 aes(fill = cat)) +
      #Manual color definitions
      scale_colour_manual(values = c(
        "Clooney" = unname(brewercolors["Orange"]),
        "Jolie" = unname(brewercolors["Blue"]),
        "No Endorser" = unname(brewercolors["Green"])
      )
      ) + 
      #Manual fill definitions
      scale_fill_manual(values = c(
        "Clooney" = unname(brewercolors["Orange"]),
        "Jolie" = unname(brewercolors["Blue"]),
        "NoEnd" = unname(brewercolors["Green"])
      )
      ) + 
      #Eta squared text label
      geom_text(label = paste0("'eta'^2 == ", format(round(etasqrd,2), nsmall = 2)),
                x = end - 0.4,
                y = 9.5,
                hjust = 0.5,
                parse = TRUE) +
      #Fvalue text label
      geom_text(label = paste0("F = ",
                               format(round(Fstat,1), nsmall = 2),
                               " (",
                               format(df1And2[1]),
                               ",",
                               format(df1And2[2]),
                               ") p <= ",
                               round(pValue,2)),
                x = end - 0.4,
                y = 9.0,
                hjust = 0.5,
                parse = FALSE) +
      #Post hoc Cloon jolie text 
      geom_text(label = paste0("p <= ",
                               phJolCloon
                               ),
                x = end - 1,
                y = mean(c(mean(subset(df, cat == "Clooney")$y),
                              mean(subset(df, cat == "Jolie")$y))
                              ),
                hjust = 0,
                parse = FALSE) +
      #Post hoc Cloon NoEnd text 
      geom_text(label = paste0("p <= ",
                               phNoEndJol
      ),
      x = end - 0.53,
      y = mean(c(mean(subset(df, cat == "Jolie")$y),
                 mean(subset(df, cat == "NoEnd")$y))
      ),
      hjust = 0,
      parse = FALSE) +
      #Post hoc Jolie NoEnd text 
      geom_text(label = paste0("p <= ",
                               phNoEndCloon
      ),
      x = end - 0.08,
      y = mean(c(mean(subset(df, cat == "Clooney")$y),
                 mean(subset(df, cat == "NoEnd")$y))
      ),
      hjust = 0,
      parse = FALSE) +
      #Legend definitions
      guides(colour = guide_legend(title = "Group Means:"),
             linetype = guide_legend(title = "Grand Mean:",label = FALSE),
             fill = FALSE) + 
      #X label definitions
      scale_x_continuous(breaks = 1:4,labels = c("Clooney", "Jolie", "No Endorser", ""), limits = c(0.5,4.15)) +
      scale_y_continuous(breaks = 1:10, limits = c(1,10)) +
      #coord_cartesian(ylim = c(min(df$y) - 1,max(df$y)+ 1)) +
      xlab("") + 
      ylab("Willingness to donate") + 
      theme_general() + 
      theme(legend.position = "top") 
    
  })
  
  
})
