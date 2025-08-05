#' Plot ActiWiz IRAS vs calculated IRAS
#'
#' Function to plot (almost) the relationship of IRAS
#' @name plot_IRAS
#' @param filename Where is the items file located and what's the name
#' @param filetype "items" or "packages" depending on the file you plot
#' @export

library(ggplot2)
library(dplyr)
library(patchwork)

plot_IRAS <- function(filename, filetype) {

  library(ggplot2)
  library(dplyr)
  library(patchwork)

  # set the proper theme
  theme_set(theme_professional())

  df <- read.csv(filename)

  plot <- ggplot()

  if (filetype=="items") {
    plot <- ggplot(df,aes(x=item_IRAS,y=IRAS))
  }
  if (filetype=="packages") {
    plot <- ggplot(df,aes(x=package_IRAS,y=IRAS))
  }

  plot <- plot +
    geom_point()+
    geom_abline(intercept = 0, slope=1,
                color="firebrick", linewidth=0.8,linetype="dashed")+
    scale_x_log10()+scale_y_log10()+
    labs(x="ActiWiz IRAS",y="Transfer Functions IRAS")

  return(plot)


}
