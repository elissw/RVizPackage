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

#' Function to calculate bin width but in a log axis
#'
#' @name freedman_diaconis_binwidth_log
#' @param x (vector) data to be plotted as histogram (eg. column of dataframe)
#' @export
#'
freedman_diaconis_binwidth_log <- function(x) {
  x <- log10(na.omit(x))  # Apply log transformation
  iqr_x <- IQR(x, na.rm = TRUE)
  n <- length(x)
  return(2 * iqr_x / (n^(1/3)))  # Convert back to linear scale
}


#' List exported functions of an installed package
#'
#' @return A character vector of function names
#' @export
list_package_functions <- function() {
  pck <- "VizPackage"
  exports <- getNamespaceExports(pkg)

  funs <- exports[sapply(exports, function(x) {
    is.function(getExportedValue(pkg, x))
  })]

  return(funs)
}
