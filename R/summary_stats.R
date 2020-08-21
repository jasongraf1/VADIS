#' Calculate summary statistics for list of models
#'
#' @param model_list a list of model objects
#' @param data_list a list od dataframes
#' @param response the names of the response column in the data
#'
#' @return a vector
#' @export
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
  if (is.na(out_df$kappa)) cat("\nTo calculate kappa scores include `x = TRUE` argument in glm()")
  if (is.na(out_df$Brier)) cat("\nBrier scores only available for probability forests.\nInclude `probability = TRUE` in ranger().")
  return(out_df)
}



