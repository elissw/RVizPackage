#' Theme function for all plots
#'
#' Create a theme with accompanying functions, so all plots
#' are homogeneous.
#' @export
theme_professional <- function() {

  library(ggplot2)

  # Choose a font, for example Calibri which
  # is probably by default installed everywhere
  # and it's a readable sans-serif font
  windowsFonts(C = windowsFont("Calibri"))
  # Start on the basis of theme_classic
  theme_classic(base_family = "C") +
    theme(
      # Try to get all bloody fonts matching but probably fail
      # and specify for each of them
      text = element_text(family = "C"),

      # Plot general
      #---------------
      # Update margins
      plot.margin = unit(c(14, 14, 14, 14), "pt"),
      # Create an existing yet not too obvious grid
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      # Title characteristics (Big, bold and centered)
      plot.title = element_text(size = 14, family = "C", face = "bold", hjust = 0.5, margin = margin(b=10)),
      plot.subtitle = element_text(size = 10, family = "C", hjust = 0.5, margin = margin(b=12), color="darkgray"),
      # Caption (if anyone ever wants a caption)
      plot.caption = element_text(size=8, family="C", face="italic", hjust = 1, margin = margin(t=10)),

      # Axes specifics
      #----------------
      # Axis labels
      axis.text = element_text(size = 12, family = "C"),
      # X and y axis titles
      axis.title.x = element_text(size = 13, family = "C", face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 13, family = "C", face = "bold", margin = margin(r = 10)),
      # Axis lines
      axis.line = element_line(linewidth=0.6),
      axis.ticks = element_line(linewidth=0.6),

      # Legend
      #--------
      legend.title = element_text(size = 12, family = "C", face = "bold"),
      legend.text = element_text(size = 11, family = "C"),

      # Customize facet title strips
      #------------------------------
      strip.text = element_text(size = 11, family = "C"),
      strip.background = element_rect(fill="white", color="gray40", linewidth=0.2),
      panel.spacing = unit(14, "pt")

    )
} # Theme function

#' Theme that deletes the x axis of a plot
#'
#' @export
theme_no_x_axis <- theme(axis.line.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.text.x = element_blank(),
                         axis.title.x = element_blank())
#' Theme that deletes the y axis of a plot
#'
#' @export
theme_no_y_axis <- theme(axis.line.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank())
#' Theme that edits both axes so they look nice for a barplot
#' No x axis and no title for y axis. Also no grid.
#'
#' @export
theme_barplot_axes <- theme(axis.line.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            panel.grid.major = element_blank())


#' Exporting plots
#'
#' Function that saves a plot, with its size depending on how many
#' subplots exist on the grid. Aim: Have the same sizing for all
#' similar plots.
#'
#' @param plot The plot we want to save
#' @param rows The number of plots in each column of the grid
#' @param columns The number of plots in each row of the grid
#' @param legend 'y/n' Whether the plot includes a legend on the side
#' @param filename The absolute path and filename of the plot
#' @export
save_plot <- function(plot,rows,columns,legend,filename) {

  library(ggplot2)

  height1 <- 1000
  height2 <- 2000
  height3 <- 3000
  height4 <- 4000
  width1 <- 1400
  width2 <- 2800
  width3 <- 4200
  width4 <- 5600
  legend_offset <- 466

  width <- get(paste0("width",columns))
  height <- get(paste0("height",rows))

  if (legend=="y"){
    width <- width + legend_offset
  }

  ggsave(plot=plot,filename=filename,width=width,height=height,unit="px")

} # End of function to save plot to png
