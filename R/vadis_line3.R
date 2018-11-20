#' Calculate the third line of evidence for the VADIS method
#'
#' @param mod_list A list of random forest model objects.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param conditional logical. Should unconditional (default) or conditional permutation variable importance be computed. Only applies to \code{RandomForest-class} models from the \code{\link[party]{party}} package.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
#'
#' @return A list of length 3
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
#' line3 <- calc_line3(rm_list, path = FALSE)
#' }
vadis_line3 <- function(mod_list, path = NULL, conditional = FALSE){
  output_list <- vector("list")
  raw_tab <- create_rank_table(mod_list, conditional = conditional) # call function to create varimp rankings
  output_list[[1]] <- raw_tab

  cor_mat <- cor(raw_tab, method = "spearman")
  dist_mat <- 1 - cor_mat
  output_list[[2]] <- as.dist(dist_mat)

  sim_tab <- cor_mat %>%
    reshape2::melt(id.vars = NULL) %>%
    group_by(Var1) %>%
    dplyr::filter(value < 1) %>%
    summarise(Similarity = mean(value, na.rm = T))

  output_list[[3]] <- as.data.frame(sim_tab)

  names(output_list) <- c("rank.table", "distance.matrix", "similarity.coefs")

  if (path == FALSE) {
    return (output_list)
  } else if (is.null(path)) {
    path <- paste0(getwd(), "/", "vadis_line3_output", format(Sys.time(), "%Y-%b-%d_%H.%M"), ".rds")
    saveRDS(output_list, file = path)
    return (output_list)
  } else {
    saveRDS(output_list, file = path)
    return (output_list)
  }
}
