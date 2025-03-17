#' Plot all scenario parameters
#'
#' Function to plot (almost) all scenario parameters in one grid
#' @param filename Where is the scenarios file located and what's the name
#' @export
#'
library(ggplot2)
library(forcats)
library(ggridges)

plot_scenarios <- function(filename) {

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


} # end of function












