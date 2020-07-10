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
    out_df <- data.frame(matrix(nrow = length(model_list), ncol = 8)) # regression
    names(out_df) <- c("N", "baseline", "predicted.corr", "Brier",  "C", "AIC", "Max.VIF",
      "kappa", "HosLem.p")
    if (is.na(out_df$kappa)) msg <- c(msg, "To calculate kappa scores include `x = TRUE` argument in glm()")
  } else if (mclass == "brmsfit") {
    out_df <- data.frame(matrix(nrow = length(model_list), ncol = 8)) # regression
    names(out_df) <- c("N", "baseline", "predicted.corr", "Brier", "C", "WAIC",
                       "elpd_loo", "p_loo")
  } else if (mclass %in% c("ranger", "RandomForest", "randomForest")){
    out_df <- data.frame(matrix(nrow = length(model_list), ncol = 4)) # random forests
    names(out_df) <- c("N", "baseline", "predicted.corr", "Brier", "C")
    if (is.na(out_df$Brier)) msg <- c(msg, "Brier scores only available for probability forests.\nInclude `probability = TRUE` in ranger().")
  } else stop(paste("I don't recognize this model class:", mclass))

  rownames(out_df) <- names(model_list) # add row names
  # loop through models
  for(i in seq_along(model_list)){
    mod <- model_list[[i]]
    df <- data_list[[i]]
    out_df[i, ] <- calc_mod_stats(mod, df, response)
  }
  cat(paste(msg, collapse = "\n"))
  return(out_df)
}



