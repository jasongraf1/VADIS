#' Standardize regression inputs by centering around the mean and dividing by 2 standard deviations
#'
#' @param x A vector, matrix, or dataframe
#' @param cols A character vector of column names, a numeric vector of column indices, or a formula
#'
#' @return A numeric vector, a matrix, or a dataframe
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
#' x
#' stand(x)
#'
#' y <- sample(c("A", "B"), 10, replace = T, prob = c(.8, .2))
#' y
#' stand(y)
#'
#' df <- data.frame(X = x, Y = y)
#' df
#' stand(df, 1:2)
#' stand(df, Y ~ X)
#' }
stand <- function(x, cols = NULL){
  # define quick standardization process
  std <- function(z) {
    if (is.numeric(z)){
      new <- (z - mean(z))/(sd(z)*2)
    } else if (nlevels(z) == 2 | length(unique(z)) == 2){
      new <- as.numeric(as.factor(z)) - mean(as.numeric(as.factor(z)))
    } else new <- z
    return(new)
  }

  # if data.frame or matix function loops through specified columns
  if (is.data.frame(x) || is.matrix(x)) {
    if(is.null(cols)) {
      stop("Columns must be specified for standardizing")
    } else if (class(cols) == "formula") {
      f_vars <- attr(terms(cols), "term.labels")
      cols <- f_vars[which(!grepl("\\|", f_vars))]
    }
    x[cols] <- lapply(x[cols], std)
  } else x <- std(x)
  return(x)
}
