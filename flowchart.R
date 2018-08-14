#Create a flow chart for selecting tests in SPSS.

#Left-hand x positions of columns.
x <- c(0.1, 0.275, 0.45, 0.6)
#Top y positions
y <- seq(from = 1, to = 0.05, length.out = 16)

#Plot
p <- ggplot() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  #header
  geom_text(aes(x = x[1], y = y[1], label = "Type of Dependent\nVariable", hjust = 0.5, vjust = 1)) +
  geom_text(aes(x = x[2], y = y[1], label = "Number of\nIndependent Variables", hjust = 0.5, vjust = 1)) +
  geom_text(aes(x = x[3], y = y[1], label = "Types of Independent \nVariables", hjust = 0.5, vjust = 1)) +
  geom_text(aes(x = x[4], y = y[1], label = "Test", hjust = 0, vjust = 1)) +
  #dichotomous outcome
  geom_segment(aes(x = x[1], xend = x[2], y = y[4], yend = y[3])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[3], yend = y[3])) +
  geom_segment(aes(x = x[1], xend = x[2], y = y[4], yend = y[4])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[4], yend = y[4])) +
  geom_segment(aes(x = x[2], xend = x[3], y = y[4], yend = y[5])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[5], yend = y[5])) +
  geom_label(aes(x = x[1], y = y[4], label = "Dichotomy\n(2 groups)", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[2], y = y[3], label = "0", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[3], label = "Binomial test, z test on proportion", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[2], y = y[4], label = "1", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[4], label = "Dichotomy", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[4], label = "Fisher exact test", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[5], label = "Categorical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[5], label = "Crosstabs chi-squared test", hjust = 0, vjust = 0.5)) +
  #categorical outcome
  geom_segment(aes(x = x[1], xend = x[2], y = y[7], yend = y[6])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[6], yend = y[6])) +
  geom_segment(aes(x = x[1], xend = x[2], y = y[7], yend = y[7])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[7], yend = y[7])) +
  geom_label(aes(x = x[1], y = y[7], label = "Categorical\n(3+ groups)", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[2], y = y[6], label = "0", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[6], label = "One-sample chi-squared test", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[2], y = y[7], label = "1", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[7], label = "Dichotomy/Categorical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[7], label = "Crosstabs chi-squared test", hjust = 0, vjust = 0.5)) +
  #numerical outcomes
  geom_segment(aes(x = x[1], xend = x[2], y = y[12], yend = y[8])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[8], yend = y[8])) +
  geom_segment(aes(x = x[1], xend = x[2], y = y[12], yend = y[12])) +
  geom_segment(aes(x = x[2], xend = x[3], y = y[12], yend = y[10])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[10], yend = y[9])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[10], yend = y[10])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[10], yend = y[11])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[12], yend = y[12])) +
  geom_segment(aes(x = x[2], xend = x[3], y = y[12], yend = y[13])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[13], yend = y[13])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[13], yend = y[14])) +
  geom_segment(aes(x = x[1], xend = x[2], y = y[12], yend = y[15])) +
  geom_segment(aes(x = x[2], xend = x[4], y = y[15], yend = y[15])) +
  geom_segment(aes(x = x[2], xend = x[3], y = y[15], yend = y[16])) +
  geom_segment(aes(x = x[3], xend = x[4], y = y[16], yend = y[16])) +
  geom_label(aes(x = x[2], y = y[8], label = "0", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[8], label = "One-sample t test, Bootstrap one median", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[9], label = "Independent samples t test (means)", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[10], label = "Dichotomy", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[10], label = "Paired-samples t test (means)", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[11], label = "Levene's F test (variances)", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[1], y = y[12], label = "Numerical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[2], y = y[12], label = "1", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[12], label = "Categorical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[12], label = "F test one-way ANOVA", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[13], label = "Numerical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[13], label = "Test on (Spearman/Pearson) correlation", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[14], label = "Regression coefficient t test", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[2], y = y[15], label = "2+", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[15], label = "Just categorical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[15], label = "F test on two-way/multi-way ANOVA", hjust = 0, vjust = 0.5)) +
  geom_label(aes(x = x[3], y = y[16], label = "Also numerical", hjust = 0.5, vjust = 0.5)) +
  geom_label(aes(x = x[4], y = y[16], label = "F test multiple regression model, \n t tests on regression coefficients", hjust = 0, vjust = 0.8)) +
  scale_x_continuous(name = "", breaks = NULL) +
  scale_y_continuous(name = "", breaks = NULL) +
  theme_minimal()