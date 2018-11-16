#' Calculate condition number kappa
#'
#' Calculates the condition number with the intercept included, following Belsley, Kuh and Welsch (1980).
#'
#' Adapted from the \code{\link[languageR]{collin.fnc}} function in the \code{\link[languageR]{languageR}} package.
#'
#' @param mod a model object created by \code{lm, glm, lmer, glmer}
#'
#' @author F.J. Tweedie; Jason Grafmiller
#'
#' @return the condition number
#' @export
#'
#' @references Belsley, D. A. and Kuh, E. and Welsch, R. E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity, Wiley Series in Probability and Mathematical Statistics, New York.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x1 <- rnorm(100)
#' x2 <- x1+.1*rnorm(100)
#' x3 <- x2 + .5*rnorm(100)
#' y  <- sample(0:1, 100, TRUE)
#' f  <- glm(y ~ x1 + x2 + x3, family = binomial, x = TRUE)
#' calc_kappa(f)
#' }
calc_kappa <- function(mod){
  # adaption of Baayen's collin.fnc() for compatibility with current version
  # of lme4 and R 3.4
  # require(languageR, quietly = T)
  if (class(mod)[1] == "merMod"){
    data <- getME(mod, "X")
  } else if (class(mod)[1] %in% c("glm", "lm")) {
    data <- mod$x
  }
  std.fnc <- function(vec) (vec - mean(vec))/sqrt(var(vec))
  # New from R 3.4: Add as.vector() to avoid warning "Recycling array of
  # length 1 in vector array arithmetic is deprecated"
  scale.fnc <- function(vec) (vec/sqrt(as.vector(t(vec) %*% vec)))
  X = apply(data, 2, FUN = function(x) scale.fnc(as.numeric(x)))
  svd.X = svd(X, nu = 0)
  nu.X = max(svd.X$d)/svd.X$d
  kappa.X = max(nu.X)
  return(kappa.X)
}
