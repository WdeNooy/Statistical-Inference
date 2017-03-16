# Function to normalize generated values to 1-10 scale.
norm10 <- function (x, lbound1 = TRUE) {
  x <- (x - min(x)) / (max(x) - min(x))
  if (lbound1) x <- x * 9 + 1 #normalize to 1-10
  else x <- x * 10
}