#' Plot all items characteristics
#'
#' Function to plot (almost) all item characteristics in one grid
#' @name plot_items
#' @param filename Where is the items file located and what's the name
#' @export

library(ggplot2)
library(forcats)
library(dplyr)
library(patchwork)

plot_scenarios <- function(filename) {

  library(ggplot2)
  library(forcats)
  library(ggridges)
  library(dplyr)
  library(patchwork)

  # set the proper theme
  theme_set(theme_professional())

  # Read in the dataframe
  df <- read.csv(filename)



} # end





