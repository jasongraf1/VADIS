#' Calculate the first line of evidence for the VADIS method
#'
#' @param mod_list A list of regression model objects.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
#'
#' @return A list of length 3.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lm_fnc <- function(x) lm(Sepal.Length ~ Petal.Length + Petal.Width + Sepal.Width, data = x)
#' rm_list <- fit.vadis.RM(iris, split.by = "Species", fit.func = lm_fnc,
#'   path = FALSE)
#' summary(rm_list[[1]])
#'
#' line1 <- calc_line1(rm_list, path = FALSE)
#' }
vadis_line1 <- function(mod_list, path = NULL){

  output_list <- vector("list")
  raw_tab <- create_signif_table(mod_list)
  output_list[[1]] <- raw_tab

  dist_mat <- dist(t(raw_tab[-1,]), method = "euclidean")^2 # omit intercept
  output_list[[2]] <- dist_mat/nrow(raw_tab[-1,]) # normalize by number of constraints

  dist_mat2 <- (nrow(raw_tab[-1,]) - dist_mat)/nrow(raw_tab[-1,])

  sim_tab <- dist_mat2 %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column("variety") %>%
    pivot_longer(-variety) %>%
    group_by(name) %>%
    dplyr::filter(value > 0) %>%
    summarise(Similarity = mean(value, na.rm = T))

  output_list[[3]] <- as.data.frame(sim_tab)

  names(output_list) <- c("signif.table", "distance.matrix", "similarity.scores")

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line2_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".rds")
    saveRDS(output_list, file = path) }
  else if (is.character(path)) {
    saveRDS(output_list, file = path)
  }
  return (output_list)
}



