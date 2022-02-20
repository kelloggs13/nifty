
#' Plot Heatmap
#'
#' @param df todo
#' @param var1 todo
#' @param var2 todo
#' @param perc todo
#' @param title todo
#' @param legendtitle todo
#' @param col.low todo
#' @param col.high todo
#'
#' @return returns a ggplot2 object
#'
#' @examples
#' require(ggplot2)
#' data(diamonds)
#' df.plot <- diamonds %>% group_by(color, cut) %>% summarise(nobs=n()) %>%
#'   ungroup() %>% mutate(percobs = nobs / sum(nobs))
#' plotHeatmap(df.plot, "color", "cut", "percobs")


plotHeatmap <- function(df
                          , var1
                          , var2
                          , perc
                          , title = paste("Heatmap - ", var1, "vs", var2)
                          , legendtitle = "Percentage of\nObservations"
                          , col.low = "white"
                          , col.high = "red"
                          )

# Creates a heatmap that shows the cross-tabulation of two variables within a data.frame.
# The data must already be aggregated, cmp. sample call below. For using unaggregated data see plotHeatmapAggr.R
# The function returns a ggplot2 object which can then be modified further on the fly using standard ggplot2 functions.

# Sample Call:
#  df.plot <- diamonds %>% group_by(color, cut) %>% summarise(nobs=n()) %>%
#   ungroup() %>% mutate(percobs = nobs / sum(nobs))
#  plotHeatmap(df.plot, "color", "cut", "percobs")
#  plotHeatmap(df.plot, "color", "cut", "percobs", col.low = "blue", col.high = "yellow")

{

  # required packages
  # -----------------

  require(ggplot2)
  require(scales)
  require(tidyverse)

  # helper function
  # ---------------

  `%!in%` <- Negate(`%in%`)

  # create labels for percentages in each tile
  # ------------------------------------------

  df <- df %>% mutate(perc.plot = percent(df[[perc]], accuracy = 1,format ="f", formatter = "formatC"))

  # plot heatmap
  # ------------

  p <- ggplot(df, aes_string(var1, var2)) +
       geom_tile(aes_string(fill=perc), col = "white") +
       geom_text(aes(label=perc.plot)) +
       theme(panel.background = element_rect(fill = "white")
                         , axis.ticks = element_blank()
              ) +
       guides(fill=guide_legend(title=legendtitle)) +
       scale_fill_gradient(low = col.low, high = col.high, labels = scales::percent) +
       ggtitle(title)

  return(p)
}



