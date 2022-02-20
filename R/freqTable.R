

#' Frequency table
#'
#' @description Creates 1-dimensional or 2-dimensional frequency tables
#' @param df a data.frame containing var1 and var2
#' @param var1 variable to count observations by
#' @param var2 additional variable to count observations by (optional)
#'
#' @return A tibble with the number of observations per distinct value of var 1 and optionally var2
#'
#' @examples
#' data(mpg)
#' freqTable(mpg, "trans")
#' freqTable(mpg, "trans", "year")


freqTable <- function(df, var1, var2) {

  require(tidyverse)

  # freq table for 1 variable
  if(!missing(var1) & missing(var2))
  {
    df.res <- df %>%
      # number of observations
      group_by(df[[var1]]) %>% summarise(nObs = n()) %>%
      # percent of observations overall
      ungroup() %>% mutate(percObs = nObs / sum(nObs)) %>%
      # sort
      arrange(-nObs)
  }

  # freq table for 2 variables
  if(!missing(var1) & !missing(var2))
    {
    df.res <- df %>%
      # number of observations
      group_by(df[[var1]], df[[var2]]) %>% summarise(nObs = n()) %>%
      # percent of observations overall
      ungroup() %>% mutate(percObs = nObs / sum(nObs)) %>%
      # sort
      arrange(-nObs)
  }

  return(df.res)
}

