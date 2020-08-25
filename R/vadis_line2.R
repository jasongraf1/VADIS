#' Calculate the second line of evidence for the VADIS method
#'
#' @param mod_list A list of regression model objects.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
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
#' data_list <- split(particle_verbs_short, particle_verbs_short$Variety, drop = TRUE)
#'
#' fmla <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + DirObjConcreteness + DirObjThematicity + DirectionalPP + PrimeType + Semantics + Surprisal.P + Surprisal.V + Register
#'
#' glm_func <- function(x) glm(fmla, data = x, family = binomial)
#'
#' glm_list <- lapply(data_list, glm_func)
#' names(glm_list) <- names(data_list)
#'
#' line2 <- vadis_line2(glm_list, path = FALSE)
#' }
vadis_line2 <- function(mod_list, path = NULL){
  output_list <- vector("list")
  raw_tab <- create_coef_table(mod_list) # call function to create varimp rankings
  output_list[[1]] <- raw_tab

  dist_mat <- dist(t(raw_tab[-1,]), method = "euclidean") # leave out the intercept

  # get the maximum reasonable distance
  dmy <- data.frame(a = sample(c(1,-1), size = nrow(raw_tab[-1,]), replace = T))
  dmy$b <- -dmy$a # exact opposite of a
  maxD <- max(dist(t(dmy), "euclidean"))

  # save normalized distances to output
  output_list[[2]] <- dist_mat/maxD

  # Now normalize all distances to the maximum reasonable distance
  weighted_dist <- as.matrix(dist_mat/maxD)
  diag(weighted_dist) <- NA # remove diagonals before calculating means
  means <- colMeans(weighted_dist, na.rm = T)
  sim_tab <- data.frame(Similarity = 1 - means)
  rownames(sim_tab) <- names(mod_list)

  output_list[[3]] <- as.data.frame(sim_tab)

  names(output_list) <- c("coef.table", "distance.matrix", "similarity.scores")

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line2_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".rds")
    saveRDS(output_list, file = path) }
  else if (is.character(path)) {
    saveRDS(output_list, file = path)
  }
  return (output_list)
}
