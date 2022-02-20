

#' Normalize to the range [0, 1]
#'
#' @description Normalizes a numeric vector to the range [0, 1] by dividing each element of x by max(x)

#' @param x a numeric or integer vector
#'
#' @return returns the vector x where each element of x is divided by max(x)
#'
#' @details missing values in x are ignored
#'
#' @examples
#' normalizeByMax(1:5)
#' normalizeByMax(c(1:5, NA))
#' normalizeByMax(letters[1:5])

normalizeByMax <- function(x)
{
  if(is.numeric(x) | is.integer(x))
    {
      x / max(x, na.rm = TRUE)
    }
  else{stop('x must be numeric')}
}

