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
#' data_list <- split(particle_verbs_short, particle_verbs_short$Variety, drop = TRUE)
#'
#' fmla <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + DirObjConcreteness + DirObjThematicity + DirectionalPP + PrimeType + Semantics + Surprisal.P + Surprisal.V + Register
#'
#' glm_func <- function(x) glm(fmla, data = x, family = binomial)
#'
#' glm_list <- lapply(data_list, glm_func)
#' names(glm_list) <- names(data_list)
#'
#' line1 <- vadis_line1(glm_list, path = FALSE)
#' }
vadis_line1 <- function(mod_list, path = NULL, method = c("freq", "pd", "rope", "map")){

  output_list <- vector("list")
  raw_tab <- create_signif_table(mod_list, method = method)
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



