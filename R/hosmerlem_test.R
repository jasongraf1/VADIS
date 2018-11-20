#' Hosmer-Lemeshow goodness of fit test
#'
#' The Hosmer-Lemeshow test is a statistical test for goodness of fit for logistic regression models.
#'
#' @param mod a model object created by \code{lm, glm, lmer, glmer}
#' @param k number of bins to use to calculate quantiles
#'
#' @return a dataframe
#'
#' @references
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(43214)
#' x <- rnorm(100)
#' pr <- exp(x)/(1+exp(x))
#' y <- 1*(runif(100) < pr)
#' mod <- glm(y~x, family=binomial)
#' hosmerlem_test(mod, k = 10)
#' }
hosmerlem_test <- function(mod, k = 10){
  if (class(mod)[1] == "merMod"){
    obs <- lme4::getME(mod, "y")
  } else {
    obs <- mod$y
  }
  fits <- fitted(mod)
  quants <- unique(quantile(fits, probs = seq(0, 1, 1/k)))
  cuts <- cut(fits, breaks = quants, include.lowest = TRUE)
  observed <- xtabs(cbind(obs0 = 1 - obs, obs1 = obs) ~ cuts)
  expected <- xtabs(cbind(fits0 = 1 - fits, fits1 = fits) ~ cuts)
  chisq <- sum((observed - expected)^2/expected)
  pval <- 1 - pchisq(chisq, k - 2)
  dfs <- k - 2
  names(chisq) <- "X-squared"
  names(dfs) <- "df"
  return(list(statistic = chisq,
              df = dfs,
              p.value = pval,
              observed = observed,
              expected = expected))
}
