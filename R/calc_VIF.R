#' Calculate variance inflation factors for regression model parameters
#'
#' Computes variance inflation factors from the covariance matrix of parameter estimates, using the method of Davis et al. (1986), which is based on the correlation matrix from the information matrix.
#'
#' Adapted from \code{\link[rms]{vif}} in the \code{\link{rms}} package.
#'
#' @param mod a model object created by \code{lm, glm, lmer, glmer}
#'
#' @return a vector of VIFs
#'
#' @author Frank Harrell; Jason Grafmiller
#'
#' @export
#'
#' @references Davis CE, Hyde JE, Bangdiwala SI, Nelson JJ: An example of dependencies among variables in a conditional logistic regression. In Modern Statistical Methods in Chronic Disease Epidemiology, Eds SH Moolgavkar and RL Prentice, pp. 140–147. New York: Wiley; 1986.
#' @examples
#' \dontrun{
#' set.seed(1)
#' x1 <- rnorm(100)
#' x2 <- x1+.1*rnorm(100)
#' x3 <- x2 + .5*rnorm(100)
#' y  <- sample(0:1, 100, TRUE)
#' f  <- glm(y ~ x1 + x2 + x3, family = binomial)
#' calc_VIF(f)
#' }
calc_VIF <- function (mod) {
  # if (!class(mod)[1] %in% c("lmerMod", "glmerMod")) {
  #   stop("mod is not a merMod object")
  # }
  ## adapted from rms::vif
  v <- vcov(mod)
  nam <- rownames(v)
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v2 <- diag(solve(v/(d %o% d)))
  names(v2) <- nam
  return(v2)
}
