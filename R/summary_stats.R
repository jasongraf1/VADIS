#' Calculate summary statistics for list of models
#'
#' @param mlist a list of model objects
#' @param dlist a list od dataframes
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
summary_stats <- function(mlist, dlist = NULL, response = NULL){
  # Loop through models and calculate the stats
  # First create empty dataframe
  if (class(mlist[[1]])[1] %in% c("glm", "merMod")){
    out_df <- data.frame(matrix(nrow = length(mlist), ncol = 8)) # regression
    names(out_df) <- c("N", "baseline", "predicted.corr", "C", "AIC", "Max.VIF",
      "kappa", "HosLem.p")
  } else {
    out_df <- data.frame(matrix(nrow = length(mlist), ncol = 4)) # random forests
    names(out_df) <- c("N", "baseline", "predicted.corr", "C")
  }
  rownames(out_df) <- names(mlist) # add row names
  # loop through models
  for(i in seq_along(mlist)){
    mod <- mlist[[i]]
    df <- dlist[[i]]
    out_df[i, ] <- calc_mod_stats(mod, df, response)
  }
  return(out_df)
}



