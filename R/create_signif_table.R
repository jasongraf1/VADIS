#' Create Table of Significant Effects for VADIS analysis
#'
#' @param mod_list A list of regression model objects.
#' @param path Path in which to save the output (as \code{.csv} file). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param method string indicating which method to use for assessing significance from Bayesian models.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the significance values of the parameter estimates, and compiles them in a single dataframe.
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lm_fnc <- function(x) lm(Sepal.Length ~ Petal.Length + Petal.Width, data = x)
#' data_list <- split(iris, list(iris$Species), drop = T)
#' rm_list <- lapply(data_list, FUN = function(x) glm(Petal.Length ~ ., x))
#'
#' create_coef_table(rm_list)
#' }
create_signif_table <- function(mod_list, method = c("freq", "pd", "rope", "map")) {
  # identify the class of models
  type = class(mod_list[[1]])[1]
  if (type %in% c("lm", "glm", "merMod", "glmerMod")){
    sig_coef_tab <- as.data.frame(lapply(mod_list, FUN = function(m) ifelse(summary(m)$coefficients[,4] < alpha, 1, 0)))
  } else if (type[1] == "brmsfit"){
    if (method == "rope"){
      sig_coef_tab <- as.data.frame(
        lapply(mod_list,
           FUN = function(m){
             sig <- bayestestR::p_rope(m) %>%
               as.data.frame() %>%
               mutate(
                 significance = ifelse(p_ROPE < .05, 1, 0)
               ) %>%
               pull(significance)
             names(sig) <- rownames(as.data.frame(summary(m)$fixed))
             return(sig)
           }))
      names(sig_coef_tab) <- names(mod_list)
    } else if (method == "pd"){
      sig_coef_tab <- as.data.frame(
        lapply(mod_list,
               FUN = function(m){
                 sig <- bayestestR::p_direction(m) %>%
                   as.data.frame() %>%
                   mutate(
                     significance = ifelse(pd > .975, 1, 0)
                   ) %>%
                   pull(significance)
                 names(sig) <- rownames(as.data.frame(summary(m)$fixed))
                 return(sig)
               }))
      names(sig_coef_tab) <- names(mod_list)
    } else if (method == "map"){
      sig_coef_tab <- as.data.frame(
        lapply(mod_list,
               FUN = function(m){
                 sig <- bayestestR::p_map(m) %>%
                   as.data.frame() %>%
                   mutate(
                     significance = ifelse(p_MAP < .05, 1, 0)
                   ) %>%
                   pull(significance)
                 names(sig) <- rownames(as.data.frame(summary(m)$fixed))
                 return(sig)
               }))
      names(sig_coef_tab) <- names(mod_list)
    } else {
      sig_coef_tab <- as.data.frame(
        lapply(mod_list,
        FUN = function(m){
          fixed <- as.data.frame(summary(m)$fixed)
          sig <- rep(0, nrow(fixed))
          sig[fixed$`l-95% CI` > 0 & fixed$`u-95% CI` > 0] <- 1
          sig[fixed$`l-95% CI` < 0 & fixed$`u-95% CI` < 0] <- 1
          names(sig) <- rownames(fixed)
          return(sig)
        }))
    }
    names(sig_coef_tab) <- names(mod_list)
  } else {
    stop("I don't recognize this class of model...")}
  return (sig_coef_tab)
}
