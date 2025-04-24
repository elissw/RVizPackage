#' Plot all scenario parameters
#'
#' Function to plot (almost) all scenario parameters in one grid
#' @name plot_scenarios
#' @param filename Where is the scenarios file located and what's the name
#' @export

library(ggplot2)
library(forcats)
library(ggridges)
library(dplyr)
library(patchwork)
library(tidyr)
library(viridis)

plot_scenarios <- function(filename) {

  library(ggplot2)
  library(forcats)
  library(ggridges)
  library(dplyr)
  library(patchwork)
  library(tidyr)
  library(viridis)

  # set the proper theme
  theme_set(theme_professional())

  # Read in the dataframe
  df <- read.csv(filename)

  # Label the machines
  df$machine <- factor(df$machine,
                       levels = c("7000GeV", "50MeV","400GeVc","160MeV","14GeVc","1400MeV"),
                       labels = c("LHC", "Lin2","SPS","Lin4","PS","PSB"))

  # Plots
  #------
  df <- df %>%
    mutate(machine = fct_reorder(machine, table(machine)[machine]))
  plot1 <- ggplot(df,aes(y=machine))+
    geom_bar(width=0.4, fill="#26828e")+
    labs(title="Machine",y="")+
    theme_no_x_axis+
    coord_cartesian(xlim=c(0,nrow(df)*0.75))+
    geom_label(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")),
               hjust = -0.1, size = 3.5, fill = "white", label.size = 0)

  df <- df %>%
    mutate(position = fct_rev(fct_infreq(position)))
  plot2 <- ggplot(df,aes(y=position))+
    geom_bar(width=0.4, fill="#26828e")+
    labs(title="Irradiation position",y="")+
    theme_no_x_axis+
    coord_cartesian(xlim=c(0,nrow(df)*0.75))+
    geom_label(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")),
               hjust = -0.1, size = 3.5, fill = "white", label.size = 0)

  plot00 <- plot1|plot2

  df <- df %>%
    mutate(machine = fct_reorder(machine, beam_p_s, .fun = max, .desc = TRUE))

  df$facility <- df$machine
  plot1 <- ggplot(df|>filter(material=="item"),aes(x=beam_p_s,y=facility))+
    geom_density_ridges(aes(color=facility,fill=facility),
                        linewidth=0.8,alpha=0.6,
                        bandwidth = 0.25)+
    scale_x_log10()+
    scale_color_viridis_d()+scale_fill_viridis_d()+
    labs(y="Machine",x="Beam losses [p/s]")+
    guides(color="none",fill="none")

  plot2 <- ggplot(df|>filter(material=="item"),aes(x=irradiation_y,y=facility))+
    geom_density_ridges(aes(color=facility,fill=facility),
                        linewidth=0.8,alpha=0.6,
                        bandwidth = 1.3)+
    scale_color_viridis_d()+scale_fill_viridis_d()+
    labs(x="Irradiation time [y]")+
    theme_no_y_axis+
    guides(color="none",fill="none")

  plot3 <- ggplot(df|>filter(material=="item"),aes(x=waiting_y,y=facility))+
    geom_density_ridges(aes(color=facility,fill=facility),
                        linewidth=0.8,alpha=0.6,
                        bandwidth = 1.5)+
    scale_color_viridis_d()+scale_fill_viridis_d()+
    labs(x="Waiting time [y]")+
    theme_no_y_axis+
    guides(color="none",fill="none")

  plot01 <- plot1 | plot2 | plot3

  binwidth <- freedman_diaconis_binwidth((df |> filter(material == "item"))$mass_kg)
  plot1 <- ggplot(df |> filter(material == "item"), aes(x = mass_kg)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    labs(x="Mass [kg]", y="Number of items")

  df <- df|>mutate(volume_m3 = volume_cm3 / 1000000)
  binwidth <- freedman_diaconis_binwidth((df |> filter(material == "item"))$volume_m3)
  plot2 <- ggplot(df |> filter(material == "item"), aes(x = volume_m3)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    labs(x="Volume [m3]", y="Number of items")

  binwidth <- freedman_diaconis_binwidth((df |> filter(material == "item"))$density_g_cm3)
  plot3 <- ggplot(df |> filter(material == "item"), aes(x = density_g_cm3)) +
    geom_step(stat="bin", binwidth=binwidth, boundary=0, position=position_nudge(x = -binwidth/2),
              color = "black", linewidth = 0.8)+
    labs(x="Density [g/cm3]", y="Number of items")

  plot02 <- plot1|plot2|plot3

  plot_1 <- plot00/plot02/plot01
  print(plot)


  ### Materials details
  ###====================

  dummy <- data.frame()

  df <- df |> filter(material=="item")
  df_materials <- df |>
    select(thickness_cm:Ag) |>
    select(-c(thickness_cm,Ag))

  df_main_materials <- df_materials |>
    select(!ends_with("_group")) |>
    pivot_longer(cols=everything(),
                  names_to="material",
                  values_to="percentage")

  main_mats <- unique(df_main_materials$material)

  palette <- viridis(length(main_mats), option = "D")

  plot1 <- ggplot(df_main_materials,
                  aes(x = percentage))

  i <- 0
  for (material in main_mats) {
    i <- i+1
    plot1 <- plot1 +
      geom_density(data=df_main_materials|>filter(material==!!material),
                   aes(y = ..density.. / max(..density..)),
                       color=palette[i], fill=palette[i],
                       alpha=0.6, linewidth=0.8)+
      geom_text(data=dummy,
                x=0.6,y=0.95-0.07*i,color=palette[i],
                label=paste0("- ",material),
                size=4, hjust=0)
    }


  plot1 <- plot1 +
          scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))+
          labs(x="Percentage [%]", y="Percentage distribution [a.u.]",
               fill="Main material",color="Main material")


  df_material_types <- df_materials %>%
    select(ends_with("_group")) %>%
    pivot_longer(cols = everything(), names_to = "Material", values_to = "Material_Type")
  df_material_types <- df_material_types %>%
    mutate(Material = gsub("_group", "", Material))
  plot2 <- ggplot(df_material_types, aes(y = Material_Type)) +
    geom_bar(width=0.4) +
    labs(title = "Material Types",
         x="Number of items",
         y="")+
    facet_wrap(~Material, scales="free")

  plot_2 <- plot1 / plot2

  plots <- list()
  plots[[1]] <- plot_1
  plots[[2]] <- plot_2
  return(plots)

} # end of function












