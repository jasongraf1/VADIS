#' Standardize regression inputs by centering around the mean and dividing by 2 standard deviations
#'
#' @param x A vector
#'
#' @return A numeric vector
#'
#' @author Jason Grafmiller
#'
#' @details Inspired by the \code{\link[arm]{standardize}} function in the \code{arm} package. Numeric predictors are rescaled by subtracting the mean  and dividing by 2 standard deviations. Binary predictors are rescaled to have a mean of 0 and a difference of 1 between their two levels. Non-numeric variables with more than two values are unchanged.
#'
#' @seealso \code{\link[arm]{standardize}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(10, 5, 3)
#' stand(x)
#'
#' y <- sample(c("A", "B"), 10, replace = T, prob = c(.8, .2))
#' stand(y)
#' }
stand <- function(x){
  if (is.numeric(x)){
    new <- (x - mean(x))/(sd(x)*2)
    } else if (nlevels(x) == 2 | length(unique(x)) == 2){
      new <- as.numeric(as.factor(x)) - mean(as.numeric(as.factor(x)))
    } else new <- x
  return(new)
}



