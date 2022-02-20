
#' Plot Heatmap including pre-aggregation of data
#'
#' @param df x
#' @param var1 x
#' @param var2 x
#' @param col.low x
#' @param col.high x
#' @param title x
#' @param legendtitle x
#' @param var1.breaks x
#' @param var2.breaks x
#'
#' @return returns a ggplot2 object
#'
#' @examples
#' require(ggplot2)
#' data(diamonds)
#'
#' plotHeatmapAggr(diamonds
#'                 , var1 = "color"
#'                 , var2 = "cut"
#'                 , legendtitle = "Percentage of\nObservations"
#' )


plotHeatmapAggr <- function(df
                          , var1
                          , var2
                          , col.low = "white"
                          , col.high = "red"
                          , title = paste("Heatmap - ", var1, "vs", var2)
                          , legendtitle = "Percentage of\nObservations"
                          , var1.breaks
                          , var2.breaks
                          )

# Creates a heatmap that shows the cross-tabulation of two variables within a data.frame.
# The functions accepts factors and numeric/integer variables, the latter are binned automatically,
#  and it returns a ggplot2 object which can then be modified further on the fly using standard ggplot2 functions.

# Sample Call:
# data(mtcars)
# PlotHeatmap(mtcars, "mpg", "qsec")

{

  # required packages
  # -----------------

  require(ggplot2)
  require(scales)
  require(tidyverse)

  # helper function
  # ---------------

  `%!in%` <- Negate(`%in%`)

  # check for valid data types of var1 and var2
  # -------------------------------------------

  if(!is.numeric(df[[var1]]) & !is.integer(df[[var1]]) & !is.factor(df[[var1]]) & !is.ordered(df[[var1]])
     | !is.numeric(df[[var2]]) & !is.integer(df[[var2]]) & !is.factor(df[[var2]]) & !is.ordered(df[[var2]])
     ){
    stop("var1 and var2 must be numeric, integer, factor or ordered")
  }

  # pre-process variables
  # ---------------------

  # variable 1

  if(is.numeric(df[[var1]]) | is.integer(df[[var1]])) {
    if(missing(var1.breaks)){
      df <- df %>% mutate(var1.grp = cut(df[[var1]]
                                         , breaks = quantile(.data[[var1]], seq(0, 1, 0.20))
                                         , include.lowest = TRUE
                                         , dig.lab = 5)
      )
    }
    if(!missing(var1.breaks)){
      df <- df %>% mutate(var1.grp = cut(df[[var1]]
                                         , breaks = var1.breaks
                                         , include.lowest = TRUE
                                         , dig.lab = 5)
      )
    }
  }

  if(is.factor(df[[var1]]) | is.ordered(df[[var1]])){
    df <- df %>% mutate(var1.grp = .data[[var1]])
  }

  # variable 2

  if(is.numeric(df[[var2]]) | is.integer(df[[var2]])) {
    if(missing(var2.breaks)){
    df <- df %>% mutate(var2.grp = cut(df[[var2]]
                                       , breaks = quantile(.data[[var2]], seq(0, 1, 0.20))
                                       , include.lowest = TRUE
                                       , dig.lab = 5)
    )
    }
    if(!missing(var2.breaks)){
      df <- df %>% mutate(var2.grp = cut(df[[var2]]
                                         , breaks = var2.breaks
                                         , include.lowest = TRUE
                                         , dig.lab = 5)
      )
    }

  }

  if(is.factor(df[[var2]]) | is.ordered(df[[var2]])){
    df <- df %>% mutate(var2.grp = .data[[var2]])
  }

  # calculate percentages in the crosstabulation of var1~var2
  # ---------------------------------------------------------

  plot.df <- df %>%
    group_by(var1.grp, var2.grp) %>%
    summarise(n=n()) %>%
    ungroup() %>% mutate(perc = n/sum(n) )

  # create labels for percentages in each tile
  # ------------------------------------------

  plot.df <- plot.df %>% mutate(perc.plot = percent(perc, accuracy = 1,format ="f", formatter = "formatC"))

  # plot heatmap
  # ------------

  p <- ggplot(plot.df, aes(var1.grp, var2.grp)) +
    geom_tile(aes(fill=perc), col = "white") +
    geom_text(aes(label=perc.plot)) +
    theme(panel.background = element_rect(fill = "white")
          , axis.ticks = element_blank()
    ) +
    guides(fill=guide_legend(title=legendtitle)) +
    scale_fill_gradient(low = col.low, high = col.high, labels = scales::percent) +
    ggtitle(title)


  return(p)
}





