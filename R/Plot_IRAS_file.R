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

  # Create dummy dataframe for annotations
  dummy <- data.frame()

  # set the proper theme
  theme_set(theme_professional())

  df <- read.csv(filename)

  plot <- ggplot()

  if (filetype=="items") {
    plot <- ggplot(df,aes(x=item_IRAS,y=IRAS))
    # Add column with IRAS ratio
    df <- df |> mutate(IRAS_ratio = IRAS/item_IRAS)
  }
  if (filetype=="packages") {
    plot <- ggplot(df,aes(x=package_IRAS,y=IRAS))
    # Add column with IRAS ratio
    df <- df |> mutate(IRAS_ratio = IRAS/package_IRAS)
  }

  label <- mean(df$IRAS_ratio)

  plot <- plot +
    geom_point()+
    geom_abline(intercept = 0, slope=1,
                color="firebrick", linewidth=0.8,linetype="dashed")+
    scale_x_log10()+scale_y_log10()+
    labs(x="ActiWiz IRAS",y="Transfer Functions IRAS")+
    geom_text(data=dummy,
              aes(x=1e1,y=1e-1),
              label=paste0("TF/AW = ",round(label,2)))


  return(plot)


}
