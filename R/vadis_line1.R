#' Calculate the first line of evidence for the VADIS method
#'
#' @param mod_list A list of regression model objects.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param alpha The significance threshold. Default is .05
#' @param method The method for calculating significance values. See details.
#' @param overwrite Should the function overwrite data to location in \code{path}? Default is \code{'no'}, which will run the analysis if no file exists. If file in \code{path} exists, user with be prompted to set new path or allow file to be overwritten. Set to \code{'yes'} to automatically overwrite existing file, and \code{'reload'} to automatically reload existing file.
#' @param verbose Should messages be printed? Default is \code{FALSE}
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe. There are four possible values for the \code{method} argument: \code{"freq"}, \code{"pd"}, \code{"rope"}, \code{"map"}. For frequentist (non-Bayesian) models, only the standard outputs of \code{glm} and \code{glmer} are used, i.e. \code{method = "freq"}.
#'
#' For Bayesian models, all four methods are available. For \code{method = "freq"}, significance is determined based on the Highest Posterior Density Interval (HDI), which is determined as 1 - \code{alpha}. Significance is defined as whether the HDI contains 0. For \code{method = "pd"} ("probability of direction"), the p-value is defined as the proportion of the posterior distribution that is of the median’s sign. In other words, the p-value represents the proportion of the posterior distribution that is above/below 0, whichever is larger). For \code{method = "rope"}, the p-value is defined as the proportion of the entire posterior distribution that lies within the Region of Practical Equivalence (ROPE), which is defined here as \code{c(-0.1, 0.1)} (see Kruschke \& Liddell 2018; Makowski et al. 2019). For \code{method = "map"}, the p-value is defined as the density value at 0 divided by the density at the Maximum A Posteriori (MAP). See \code{\link[bayestestR]{p_direction}}, \code{\link[bayestestR]{p_rope}}, and \code{\link[bayestestR]{p_map}} for further details.
#'
#' @return A \code{list} of length 3.
#' \describe{
#' \item{\code{signif.table}}{A dataframe of \emph{P} predictors by \emph{M} models, containing a binary value indicating statistical significance (1 = significant) for each predictor in each model.}
#' \item{\code{distance.matrix}}{An \emph{M} by \emph{M} distance matrix of class \code{dist}, derived from \code{signif.table}. Values are squared Euclidean distances normalized by the number of predictors \emph{P}.}
#' \item{\code{similarity.scores}}{A dataframe of similarity scores derive from \code{distance.matrix}. See Szmrecsanyi et al. (2019) for details.}
#' }
#'
#' @references
#'
#' Kruschke, John K. & Torrin M. Liddell. 2018. The Bayesian New Statistics: Hypothesis testing, estimation, meta-analysis, and power analysis from a Bayesian perspective. \emph{Psychonomic Bulletin & Review} 25(1). 178–206. doi: 10.3758/s13423-016-1221-4.
#'
#' Makowski, Dominique, Mattan S. Ben-Shachar, S. H. Annabel Chen & Daniel Lüdecke. 2019. Indices of effect existence and significance in the Bayesian framework. \emph{Frontiers in Psychology}. 10. doi: 10.3389/fpsyg.2019.02767.
#'
#' Szmrecsanyi, Benedikt, Jason Grafmiller & Laura Rosseel. 2019. Variation-Based Distance and Similarity Modeling: A Case Study in World Englishes. \emph{Frontiers in Artificial Intelligence} 2. \url{https://doi.org/10.3389/frai.2019.00023}.
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
vadis_line1 <- function(mod_list, path = NULL, alpha = .05, method = c("freq", "pd", "rope", "map"), overwrite = c("no", "yes",  "reload"), verbose = FALSE){

  p_method <- match.arg(method)
  overwrite <- match.arg(overwrite)

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line1_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".rds")
    }

  if(path == FALSE){
    output_list <- vector("list")
    raw_tab <- create_signif_table(mod_list, alpha = alpha, method = p_method)
    output_list[[1]] <- raw_tab

    dist_mat <- dist(t(raw_tab[-1,]), method = "euclidean")^2 # omit intercept

    output_list[[2]] <- dist_mat/nrow(raw_tab[-1,]) # normalize by number of constraints

    dist_mat2 <- (nrow(raw_tab[-1,]) - dist_mat)/nrow(raw_tab[-1,])

    sim_dist <- as.matrix(dist_mat2)
    diag(sim_dist) <- NA # remove diagonals before calculating means
    means <- colMeans(sim_dist, na.rm = T)
    sim_tab <- data.frame(Similarity = means)
    rownames(sim_tab) <- names(mod_list)

    output_list[[3]] <- as.data.frame(sim_tab)

    names(output_list) <- c("signif.table", "distance.matrix", "similarity.scores")
  } else if(overwrite == "reload" & file.exists(path)){
    # reload from existing file
    if(verbose) message(paste("Loading existing file", path, "\nSet `overwrite = 'yes' or choose new path to calculate new values."))
    output_list <- readRDS(path)
  } else {
    output_list <- vector("list")
    raw_tab <- create_signif_table(mod_list, alpha = alpha, method = p_method)
    output_list[[1]] <- raw_tab

    dist_mat <- dist(t(raw_tab[-1,]), method = "euclidean")^2 # omit intercept

    output_list[[2]] <- dist_mat/nrow(raw_tab[-1,]) # normalize by number of constraints

    dist_mat2 <- (nrow(raw_tab[-1,]) - dist_mat)/nrow(raw_tab[-1,])

    sim_dist <- as.matrix(dist_mat2)
    diag(sim_dist) <- NA # remove diagonals before calculating means
    means <- colMeans(sim_dist, na.rm = T)
    sim_tab <- data.frame(Similarity = means)
    rownames(sim_tab) <- names(mod_list)

    output_list[[3]] <- as.data.frame(sim_tab)

    names(output_list) <- c("signif.table", "distance.matrix", "similarity.scores")
  }

  if(is.character(path)){
    if(overwrite == "yes"){
      if(file.exists(path) & verbose == TRUE) message("Existing file", path, "will be overwritten. Set `overwrite = 'reload'` to reload existing file.")
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



