# Function to normalize generated values to 1-10 scale.
norm10 <- function (x) {
  x <- (x - min(x)) / (max(x) - min(x)) * 9 + 1 #normalize to 1-10
}