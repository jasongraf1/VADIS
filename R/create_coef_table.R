#' Create Table of Coefficients for VADIS analysis
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
create_coef_table <- function(mod_list) {
  # identify the class of models
  type = class(mod_list[[1]])[1]
  if (type %in% c("merMod", "glmerMod")){
    coef_tab <- as.data.frame(lapply(mod_list, FUN = lme4::fixef))
  } else if (type[1] %in% c("lm", "glm", "lrm")) {
    coef_tab <- as.data.frame(lapply(mod_list, FUN = stats::coef))
  } else if (type[1] == "brmsfit"){
    coef_tab <- as.data.frame(lapply(stan_models, FUN = function(x) summary(x)$fixed[,1]))
  } else stop("I don't recognize this class of model...")
  return (coef_tab)
}
