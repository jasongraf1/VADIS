#' Retrieve random samples of posterior coefficient estimates
#'
#' @param mod_list A list of regression model objects of class \code{brmsfit}.
#' @param n Integer indicating number of samples to pull
#'
#' @return a list of dataframes
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of *M* model objects, randomly samples coefficient estimates from the posterior, and compiles those samples in a single dataframe. Output is a list of *M* dataframes each with *n* columns of sampled estimates. Currently only works with \code{brmsfit} objects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Coming soon...
#' }
get_posterior_samples <- function(mod_list, n = 200L){

  post_samples <- lapply(
    mod_list,
    FUN = function(mod){
      if (class(mod) == "glm"){
        coefs <- summary(mod)$coefficients
        est <- coefs[-1, 1]
        SEs <- coefs[-1, 2]
        coef_sample_list <- vector("list")
        # go through each predictor and get samples from the estimated distribution
        for (i in 1:length(est)){
          coef_sample_list[[i]] <- rnorm(n, mean = est[i], sd = SEs[i])
        }
        names(coef_sample_list) <- names(est)
        sample_df <- as.data.frame(do.call("rbind", coef_sample_list))
        names(sample_df) <- paste("run", 1:n, sep = "")
      } else if (class(mod) == "brmsfit"){
        sample_df <- brms::posterior_samples(mod, pars = '^b')[,-1]
        sample_df <- t(sample_df[sample(1:nrow(sample_df), n),]) # no intercept
      }
      return(sample_df)
    })
  names(post_samples) <- names(mod_list)
  return(post_samples)
}

