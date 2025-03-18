#' Function to calculate bin width
#'
#' @name freedman_diaconis_binwidth
#' @param x (vector) data to be plotted as histogram (eg. column of dataframe)
#' @export


freedman_diaconis_binwidth <- function(x) {
  iqr_x <- IQR(x, na.rm = TRUE)
  n <- length(na.omit(x))
  return (2 * iqr_x / (n^(1/3)))
}
