report_n <- function (x, digits = 2) {
  # Round to digs decimal places but preserve trailing zeros.
  # Default: round to 2 digits.
  x <- format(round(x, digits=digits), nsmall = digits)
  return(x)
}