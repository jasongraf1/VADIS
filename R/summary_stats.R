#' Calculate summary statistics for list of models
#'
#' @param model_list a list of model objects
#' @param data_list a list od dataframes
#' @param response the names of the response column in the data
#'
#' @return a dataframe with one or more of the following columns
#'
#' \itemize{
#'   \item{N}{The number of observations in the dataset}
#'   \item{baseline}{The baseline accuracy of the dataset}
#'   \item{predicted.corr}{The proportion of observations correctly predicted by the model}
#'   \item{Brier}{The Brier score of model accuracy. Only available for models that return predicted probabilities.}
#'   \item{C}{The Concordance index (see Harrell 2015:256-258)}
#'   \item{LogScore}{The cross-entropy loss, or log loss, score, which measures the performance of a classification model whose output is a probability value between 0 and 1. Only available for models that return predicted probabilities.}
#'   \item{AIC}{The Akaike Information Criterion. Only given for regression models fit with \code{glm} and \code{glmer}}
#'   \item{WAIC}{The Widely Applicable Information Criterion, or Watanabe–Akaike Information Criterion. Only given for regression models fit with \code{brm}}
#'   \item{Max.VIF}{The maximal variance inflation factor obtained from the covariance matrix of parameter estimates in the model using the method of Davis et al. (1986). An indication of multicollinearity. Only given for regression models fit with \code{glm} and \code{glmer}
#'   \item{kappa}{The condition number calculated from the model matrix (with the intercept included), following Belsley et al. (1980).  Only given for regression models fit with \code{glm} and \code{glmer}}
#'   \item{HosLem.p}{The p-value from the Hosmer-Lemeshow goodness of fit test for logistic regression. Values \emph{below} .05 indicate evidence of poor model fit. Only given for regression models fit with \code{glm} and \code{glmer}}
#'   \item{elpd_loo}{The Bayesian leave-one-out (LOO) estimate of the expected log pointwise predictive density (ELPD). See Vehtari et al. (2017) and \url{https://avehtari.github.io/modelselection/CV-FAQ.html} for details.}
#'   \item{p_loo}{}
#'   \item{looic}{}
#' }
#'
#' @export
#'
#' @references Belsley, D. A. and Kuh, E. and Welsch, R. E. 1980. \emph{Regression Diagnostics. Identifying Influential Data and Sources of Collinearity}, Wiley Series in Probability and Mathematical Statistics, New York.
#'
#' Davis, C. E., Hyde, J. E., Bangdiwala, S. I., & Nelson, J. J. 1986. An example of dependencies among variables in a conditional logistic regression. \emph{Modern statistical methods in chronic disease epidemiology} 140. 147.
#'
#' Harrell, Frank E. 2015. \emph{Regression Modeling Strategies}. 2nd edn. New York: Springer.
#'
#' Vehtari, Aki, Andrew Gelman & Jonah Gabry. 2017. Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. \emph{Statistics and Computing} 27(5). 1413–1432.
#'
#' @examples
#' \dontrun{
#' data_list <- vector("list")
#' for (i in 1:4){
#'   df <- data.frame(x1 = rnorm(100))
#'   df$x2 <- df$x1+.1*rnorm(100)
#'   df$x3 <- df$x2 + .5*rnorm(100)
#'   df$y <- rbinom(100, 1, 1/(1 + exp(-df$x1+df$x2+df$x3)))
#'  data_list[[i]] <- df
#' }
#' rm_list <- lapply(data_list, FUN = function(d) glm(y ~ ., data = d, family = binomial, x = T))
#' summary_stats(rm_list)
#' }
summary_stats <- function(model_list, data_list = NULL, response = NULL){
  # Loop through models and calculate the stats
  mclass <- class(model_list[[1]])[1]
  msg <- c()
  # First create empty dataframe
  if (mclass %in% c("glm", "glmerMod")){
    out_df <- data.frame(matrix(nrow = length(model_list), ncol = 10)) # regression
    names(out_df) <- c("N", "baseline", "predicted.corr", "Brier",  "C", "LogScore", "AIC", "Max.VIF",
      "kappa", "HosLem.p")
  } else if (mclass == "brmsfit") {
    out_df <- data.frame(matrix(nrow = length(model_list), ncol = 10)) # regression
    names(out_df) <- c("N", "baseline", "predicted.corr", "Brier", "C", "LogScore", "WAIC",
                       "elpd_loo", "p_loo", "looic")
  } else if (mclass %in% c("ranger", "RandomForest", "randomForest")){
    out_df <- data.frame(matrix(nrow = length(model_list), ncol = 6)) # random forests
    names(out_df) <- c("N", "baseline", "predicted.corr", "Brier", "C", "LogScore")
  } else stop(paste("I don't recognize this model class:", mclass))

  rownames(out_df) <- names(model_list) # add row names
  # loop through models
  for(i in seq_along(model_list)){
    mod <- model_list[[i]]
    df <- data_list[[i]]
    out_df[i, ] <- calc_mod_stats(mod, df, response)
  }
  # messages
  if ("kappa" %in% names(out_df)){
    if (is.na(out_df$kappa[1])) cat("\nTo calculate kappa scores include `x = TRUE` argument in glm()\n")
  }
  if (is.na(out_df$Brier[1])) cat("\nBrier and Log scores only available for probability forests.\nInclude `probability = TRUE` in ranger().\n")
  return(out_df)
}



