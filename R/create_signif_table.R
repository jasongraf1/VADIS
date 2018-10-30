#' Create Table of Significant Effects for VADIS analysis
#'
#' @param mod_list A list of regression model objects.
#' @param path Path in which to save the output (as \code{.csv} file). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lm_fnc <- function(x) lm(Sepal.Length ~ Petal.Length + Petal.Width, data = x)
#' rm_list <- fit.vadis.RM(iris, split.by = "Species", fit.func = lm_fnc,
#'   path = FALSE)
#' summary(rm_list[[1]])
#'
#' create_coef_table(rm_list, path = FALSE)
#' }
create_signif_table <- function(mod_list) {
  # identify the class of models
  type = class(mod_list[[1]])
  if (type[1] %in% c("lm", "glm", "merMod")){
    sig_coef_tab <- as.data.frame(lapply(mod_list,
      FUN = function(m) ifelse(summary(m)$coefficients[,4] < .05, 1, 0)))
  } else if (type[1] == "brmsfit"){
    # Extract posterior samples and look for any outside 95% HPD interval
    post_samp <- vector("list")
    for(i in 1:length(mod_list)){
      mod <- mod_list[[i]]
      post_samp[[i]] <- posterior_samples(mod, pars = '^b')
    }
    names(post_samp) <- names(mod_list)

    # create dataframe with mean posterior estimate
    sig_coef_tab <- as.data.frame(lapply(post_samp,
      .fun = function(x) apply(x, 2, mean)))
    rownames(sig_coef_tab) <- names(mod_list)
  } else stop("I don't recognize this class of model...")
  return (sig_coef_tab)
}
