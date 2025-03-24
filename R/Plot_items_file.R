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
  binwidth <- freedman_diaconis_binwidth_log((df |> filter(material == "item"))$item_DR_10cm_uSv_h)
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

  plot00 <- plot1 | plot2 | plot3


  df <- df |>
    mutate(item_classification = ifelse(
      item_DR_10cm_uSv_h < 0.1 &
        item_LL < 1.0,
      0, 1))

  plot1 <- ggplot(df,aes(y=item_classification))+
    geom_bar(width=0.4, fill="#26828e")+
    labs(title="Classification")+
    theme_barplot_axes+
    coord_cartesian(xlim=c(0,nrow(df)*0.95))+
    geom_label(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")),
              hjust = -0.1, size = 3.5, fill = "white", label.size = 0)+
    scale_y_continuous(breaks=c(0,1),labels=c("Conventional","Radioactive"))

  binwidth <- freedman_diaconis_binwidth_log((df |> filter(material == "item"))$item_LL)
  plot2 <- ggplot(df |>
                    filter(material == "item"),
                  aes(x = item_LL)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    scale_x_log10()+
    labs(x="LL", y="Number of items")+
    geom_vline(xintercept=1, linewidth=0.6, linetype="dashed",color="coral")

  binwidth <- freedman_diaconis_binwidth_log((df |> filter(material == "item"))$item_IRAS)
  plot3 <- ggplot(df |>
                    filter(material == "item"),
                  aes(x = item_IRAS)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    scale_x_log10()+
    labs(x="IRAS", y="Number of items")

  plot01 <- plot1 | plot2 | plot3


  binwidth <- freedman_diaconis_binwidth_log((df |> filter(material == "item"))$total_activity_Bq_g)
  plot1 <- ggplot(df |>
                    filter(material == "item"),
                  aes(x = total_activity_Bq_g)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    scale_x_log10()+
    labs(x="Total activity [Bq/g]", y="Number of items")

  plot2 <- ggplot(df |>
                    filter(material == "item") |>
                    mutate(position = fct_reorder(position, total_activity_Bq_g, median, .desc = FALSE)),
                  aes(x = total_activity_Bq_g, y = position)) +
    geom_boxplot(alpha=0.6) +
    scale_x_log10()+
    labs(x="Total activity [Bq/g]", y="Irradiation position")

  plot3 <- ggplot(df |>
                    mutate(material = fct_reorder(material, total_activity_Bq_g, median, .desc = FALSE)),
                  aes(x = total_activity_Bq_g, y = material)) +
    geom_boxplot(alpha=0.6) +
    scale_x_log10()+
    labs(x="Total activity [Bq/g]", y="Item component")

  plot02 <- plot1 | plot2 | plot3


  plot <- plot01/plot00/plot02
  return(plot)



} # end





