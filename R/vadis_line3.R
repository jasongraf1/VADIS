#' Calculate the third line of evidence for the VADIS method
#'
#' @param mod_object Either a list of random forest model objects, or a dataframe of variable importance scores, where rows represent predictors in the models and columns represent the varieties to compare.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param conditional logical. Should conditional (default) or unconditional permutation variable importance be computed. Only applies to \code{RandomForest-class} models from the \code{\link[party]{party}} package.
#' @param overwrite Should the function overwrite data to location in \code{path}? Default is \code{'no'}, which will run the analysis if no file exists. If file in \code{path} exists, user with be prompted to set new path or allow file to be overwritten. Set to \code{'yes'} to automatically overwrite existing file, and \code{'reload'} to automatically reload existing file.
#' @param verbose Should messages be printed? Default is \code{FALSE}
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
#'
#' @return A \code{list} of length 4.
#' \describe{
#' \item{\code{varimp.table}}{A dataframe of \emph{P} predictors by \emph{M} random forest models, containing the variable importance scores obtained for each predictor in the models. }
#' \item{\code{rank.table}}{A dataframe of \emph{P} predictors by \emph{M} random forest models, containing the rankings of predictors in each model by variable importance score in \code{varimp.table}.}
#' \item{\code{distance.matrix}}{An \emph{M} by \emph{M} distance matrix of class \code{dist}, derived from \code{rank.table}. Values are calculated as 1 - the Spearman rank correlation \rho.}
#' \item{\code{similarity.scores}}{A dataframe of similarity scores derive from \code{distance.matrix}. See Szmrecsanyi et al. (2019) for details.}
#' }
#'
#' @references Szmrecsanyi, Benedikt, Jason Grafmiller & Laura Rosseel. 2019. Variation-Based Distance and Similarity Modeling: A Case Study in World Englishes. \emph{Frontiers in Artificial Intelligence} 2. \url{https://doi.org/10.3389/frai.2019.00023}.
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
vadis_line3 <- function(mod_object, path = NULL, conditional = TRUE, overwrite = c("no", "yes", "reload"), verbose = FALSE){

  overwrite <- match.arg(overwrite)

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line3_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".rds")
    }

  if(path == FALSE){
    output_list <- vector("list")
    if (is.data.frame(mod_object) || is.matrix(mod_object)){
      raw_tab <- mod_object
    } else if (is.list(mod_object)){
      if(conditional == TRUE & class(mod_object[[1]]) != "RandomForest"){
        conditional <- FALSE
        if(verbose) message("Conditional variable importances are only available for {party} random forests. Setting conditional = FALSE")
      }
      raw_tab <- create_rank_table(mod_object, conditional = conditional) # call function to create varimp rankings
    } else {
      stop(paste("Function does not work with objects of class", class(mod_object)[1]))
    }

    output_list[[1]] <- raw_tab

    rank_tab <- as.data.frame(apply(raw_tab, 2, function(x) rank(-x, ties.method = "min")))
    rownames(rank_tab) <- rownames(raw_tab)
    output_list[[2]] <- rank_tab

    cor_mat <- cor(raw_tab, method = "spearman")
    dist_mat <- 1 - cor_mat

    output_list[[3]] <- as.dist(dist_mat)

    diag(cor_mat) <- NA
    means <- colMeans(cor_mat, na.rm = T)
    sim_tab <- data.frame(Similarity = means)
    rownames(sim_tab) <- names(mod_object)

    output_list[[4]] <- sim_tab

    names(output_list) <- c("varimp.table",
                            "rank.table",
                            "distance.matrix",
                            "similarity.scores")
  } else if(overwrite == "reload" & file.exists(path)){
    # reload from existing file
    if(verbose) message(paste("Loading existing file", path, "\nSet `overwrite = 'yes' or choose new path to calculate new values."))
    output_list <- readRDS(path)
  } else {
    output_list <- vector("list")
    if (is.data.frame(mod_object) || is.matrix(mod_object)){
      raw_tab <- mod_object
    } else if (is.list(mod_object)){
      raw_tab <- create_rank_table(mod_object, conditional = conditional) # call function to create varimp rankings
    } else {
      stop(paste("Function does not work with objects of class", class(mod_object)[1]))
    }

    output_list[[1]] <- raw_tab

    rank_tab <- as.data.frame(apply(raw_tab, 2, function(x) rank(-x)))
    rownames(rank_tab) <- rownames(raw_tab)
    output_list[[2]] <- rank_tab

    cor_mat <- cor(raw_tab, method = "spearman")
    dist_mat <- 1 - cor_mat

    output_list[[3]] <- as.dist(dist_mat)

    diag(cor_mat) <- NA
    means <- colMeans(cor_mat, na.rm = T)
    sim_tab <- data.frame(Similarity = means)
    rownames(sim_tab) <- names(mod_object)

    output_list[[4]] <- sim_tab

    names(output_list) <- c("varimp.table",
                            "rank.table",
                            "distance.matrix",
                            "similarity.scores")
  }

  if(is.character(path)){
    if(overwrite == "yes"){
      if(file.exists(path) & verbose == TRUE) message("Existing file", path, "will be overwritten. Set overwrite = 'reload' to reload existing file.")
      saveRDS(output_list, file = path)
    } else if(overwrite == "no" & file.exists(path)) {
      msg <- paste("File", path, "already exists. Overwrite (y/n)?: ")
      over <- readline(prompt = msg)
      if(over == "y") {
        saveRDS(output_list, file = path)
      } else {
        new_path <- readline(prompt = "Please enter new file path:")
        saveRDS(output_list, file = new_path)
      }
    } else {
      saveRDS(output_list, file = path)
    }}

  return (output_list)
}
