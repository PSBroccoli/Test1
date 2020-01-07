#' Sum of Squares
#'
#' Calculates the sum of squared deviations from mean
#'
#' @param x numeric vector
#'
#' @return number
#' @export
#'
#' @examples
#' sumsquares(rnorm(50))
sumsquares <- function(x){
  sum((mean(x))^2)
}


