



#' loadExtraPackages
#'
#' @description
#' `loadExtrapackages` loads a set of often used packages for data analysis.
#'
#' @details
#' The following packages are loaded:
#' * ggplo2
#' * tidyverse
#' * scales
#' * janitor
#' * stringr
#' * RColorBrewer
#' * lubridate
#' * rpart
#' * rpart.plot
#' * vroom
#' * sqldf
#' * gt
#' * patchwork
#'
#' All packages are loaded via `library()`, hence if a package can not be loaded the function aborts with an error.

loadExtraPackages <- function()
{
  # beautiful, beautiful plots
  library(ggplot2)

  # awesome data manipulation
  library(tidyverse)

  # just scales::label_percent makes this already worth
  library(scales)

  # a collection of helper functions like clean_names() and round_half_up()
  library(janitor)

  # string manipulations
  library(stringr)

  # color palettes
  library(RColorBrewer)

  # date manipulations
  library(lubridate)

  # fit decision trees
  library(rpart)

  # plot decision trees
  library(rpart.plot)

  # super fast way to read delimited files
  library(vroom)

  # exchange data between an sql database and R
  library(sqldf)

  # really clean tables with lots, lots, LOTS of options
  library(gt)

  # layouting multiple ggplot2 plots
  library(patchwork)
}
