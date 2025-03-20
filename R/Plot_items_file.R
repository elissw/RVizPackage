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

plot_items <- function(filename) {

  library(ggplot2)
  library(forcats)
  library(ggridges)
  library(dplyr)
  library(patchwork)

  # set the proper theme
  theme_set(theme_professional())

  # Read in the dataframe
  df <- read.csv(filename)

  # Label the machines
  df$machine <- factor(df$machine,
                       levels = c("7000GeV", "50MeV","400GeVc","160MeV","14GeVc","1400MeV"),
                       labels = c("LHC", "Lin2","SPS","Lin4","PS","PSB"))

  # Plots
  #-------
  binwidth <- freedman_diaconis_binwidth((df |> filter(material == "item"))$item_DR_10cm_uSv_h)
  plot1 <- ggplot(df |>
                    filter(material == "item"),
                  aes(x = item_DR_10cm_uSv_h)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    scale_x_log10()+
    labs(x="Dose rate @ 10 cm [uSv/h]", y="Number of items")

  plot2 <- ggplot(df |>
                   filter(material == "item") |>
                   mutate(position = fct_reorder(position, item_DR_10cm_uSv_h, median, .desc = FALSE)),
                 aes(x = item_DR_10cm_uSv_h, y = position)) +
    geom_boxplot(alpha=0.6) +
    scale_x_log10()+
    labs(x="Dose rate @ 10 cm [uSv/h]", y="Irradiation position")

  plot3 <- ggplot(df |>
                   mutate(material = fct_reorder(material, DR_10cm_uSv_h, median, .desc = FALSE)),
                 aes(x = DR_10cm_uSv_h, y = material)) +
    geom_boxplot(alpha=0.6) +
    scale_x_log10()+
    labs(x="Dose rate @ 10 cm [uSv/h]", y="Item component")

  plot <- plot1 | plot2 | plot3

  return(plot)

} # end





