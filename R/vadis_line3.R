#' Calculate the third line of evidence for the VADIS method
#'
#' @param mod_object Either a list of random forest model objects, or a dataframe of variable importance scores, where rows represent separate models and columns represent the importance values of the predictors in the models.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param conditional logical. Should conditional (default) or unconditional permutation variable importance be computed. Only applies to \code{RandomForest-class} models from the \code{\link[party]{party}} package.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
#'
#' @return A list of length 4
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_list <- split(particle_verbs_short, particle_verbs_short$Variety, drop = TRUE)
#'
#' fmla <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + DirObjConcreteness + DirObjThematicity + DirectionalPP + PrimeType + Semantics + Surprisal.P + Surprisal.V + Register
#'
#' rf_func <- function(x) ranger(fmla, data = x, importance = "permutation")
#'
#' rf_list <- lapply(data_list, rf_func)
#' names(rf_list) <- names(data_list)
#'
#' line3 <- vadis_line3(rf_list, path = FALSE)
#' }
vadis_line3 <- function(mod_object, path = NULL, conditional = TRUE){

  output_list <- vector("list")
  if (is.data.frame(mod_object) || is.matrix(mod_object)){
    raw_tab <- mod_object
  } else if (is.list(mod_object)){
    raw_tab <- create_rank_table(mod_object, conditional = conditional) # call function to create varimp rankings
  } else {
    stop(paste("Function does not work with objects of class", class(mod_object)[1]))
  }

  output_list[[1]] <- raw_tab

  rank_tab <- as.data.frame(apply(raw_tab, 1, function(x) rank(-x)))
  rownames(rank_tab) <- rownames(raw_tab)
  output_list[[2]] <- rank_tab

  cor_mat <- cor(raw_tab, method = "spearman")
  dist_mat <- 1 - cor_mat

  output_list[[3]] <- as.dist(dist_mat)

  diag(cor_mat) <- NA
  means <- colMeans(cor_mat, na.rm = T)
  sim_tab <- data.frame(Similarity = means)
  rownames(sim_tab) <- names(mod_list)

  output_list[[4]] <- sim_tab

  names(output_list) <- c("varimp.table",
                          "rank.table",
                          "distance.matrix",
                          "similarity.scores")

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line3_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".rds")
    saveRDS(output_list, file = path) }
  else if (is.character(path)) {
    saveRDS(output_list, file = path)
  }
  return (output_list)
}
