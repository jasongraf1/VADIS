#' Min-Max scaling
#'
#' @param x a numeric vector
#'
#' @description function for scaling a numeric vector to between 0 and 1
#'
#' @return a vector
#'
#' @export
#'
#' @examples
#' x <- rnorm(20)
#' minmax(x)
minmax <- function(x) {
  (x - min(x))/(max(x) - min(x))
}
