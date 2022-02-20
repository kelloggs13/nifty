


#' Sankey Diagram
#'
#' @description plots a sankey diagram
#'
#' @param df a data.frame containing the number of observations for each flow, cmp. example below
#' @param y the number of observations for each flow
#' @param vars a string with the variables used as pillars in the chart
#' @param labels.text a string with the labels used to name the groups
#' @param label.columns a string with a title to describe the pillars
#' @param perc todo
#' @param col.alluvium todo
#' @param col.stratum todo
#'
#' @return returns a ggplot2 object
#'
#' @examples
#' require(tidyverse)
#'
#' set.seed(2334567)
#' ncust <- 10000
#' df.cust <- data.frame(customerid = 1:ncust
#'                       , segment_2000 = sample(LETTERS[1:4], ncust, replace = TRUE, prob = c(0.1, 0.2, 0.5, 0.2))
#'                       , segment_2001 = sample(LETTERS[1:4], ncust, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
#' )
#'
#' df.years  <-  df.cust %>%
#'   group_by(segment_2000, segment_2001) %>% summarise(ncust = n()) %>%
#'   ungroup() %>% mutate(perccust = ncust / sum(ncust))
#'
#' df.years %>% plotSankey(y = "ncust"
#'                         , vars = c("segment_2000", "segment_2001")
#'                         , labels.text = c("2000", "2001")
#'                         , label.columns = 'Year'
#' )
#'
#' df.years %>% plotSankey(y = "perccust"
#'                         , vars = c("segment_2000", "segment_2001")
#'                         , labels.text = c("2000", "2001")
#'                         , perc = TRUE
#'                         , label.columns = 'Year'
#' )
#'
#' df.years %>% plotSankey(y = "ncust"
#'                         , vars = c("segment_2000", "segment_2001")
#'                         , labels.text = c("2000", "2001")
#'                         , label.columns = 'Year'
#'                         , col.alluvium = "blue"
#'                         , col.stratum = "grey"
#' )
#'

plotSankey <- function(df
                       , y
                       , vars
                       , labels.text
                       , label.columns
                       , perc
                       , col.alluvium
                       , col.stratum
                       )
{

  require(ggplot2)
  require(tidyverse)
  require(ggalluvial)
  require(scales)

  if(length(vars) != 2) stop('vars must contain exactly 2 variables')
  if(length(vars) != length(labels.text)) stop('length(vars) must be equal to length(labels.text)')

   p <- ggplot(df, aes_string(y = y, axis1 = vars[1], axis2 = vars[2])) +
    geom_alluvium(fill = col.alluvium) +
    geom_stratum(fill = col.stratum) +
    guides(fill="none") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3)  +
    scale_x_continuous(label.columns, breaks = 1:length(vars), labels = labels.text, position = "top") +
    { if(perc == FALSE) scale_y_continuous("Number of observations in group\n"
                                           , labels = comma
                                            , sec.axis = sec_axis(trans=~.*1, name="", labels = comma)
                                           ) } +
    { if(perc == TRUE)   scale_y_continuous("Number of observations in group\n"
                                            , labels = percent
                                            ,  sec.axis = sec_axis(trans=~.*1, name="", labels = percent)
    ) } +
    theme_minimal()

  return(p)
}
