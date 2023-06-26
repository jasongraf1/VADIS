#' Calculate the second line of evidence for the VADIS method
#'
#' @param mod_list A list of regression model objects.
#' @param path Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param weight A numeric value indicating the size of the "effects" used for approximating the maximal reasonable distance. Default is 1.
#' @param scale How should the distance matrix be scaled? See details
#' @param overwrite Should the function overwrite data to location in \code{path}? Default is \code{'reload'}, which will load as existing file in \code{path}, and run the analysis if no file exists. If 'no' and the file exists, you will be asked to enter a new file location. Set \code{'yes'} to overwrite existing file.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
#'
#' For scaling, there are four options. The default, \code{"abs"} (absolute), scales by a constant term based on the maximum reasonable distance, and values are bounded between 0 and 1 (see Szmrecsanyi et al. 2019). \code{"minmax"} uses minmax normalization, defined as
#'
#' \deqn{ x' = \frac{x - min(x)}{max(x) - min(x)}}{x' = (x - min(x))/(max(x) - min(x))}
#'
#' Minmax scaling bound values between 0 and 1. \code{"mean"} uses mean normalization, defined as

#' \deqn{ x' = \frac{x - mean(x)}{max(x) - min(x)}}{x' = (x - mean(x))/(max(x) - min(x))}
#'
#' If \code{scale = "none"} no scaling is applied.
#'
#' @return A \code{list} of length 3.
#' \describe{
#' \item{\code{coef.table}}{A dataframe of \emph{P} predictors by \emph{M} models, containing the pointwise estimated  coefficients (for \code{glm} and \code{glmer} models) or the mean posterior \beta estimates (for \code{brmsfit} models) for each predictor in each model.}
#' \item{\code{distance.matrix}}{An \emph{M} by \emph{M} distance matrix of class \code{dist}, derived from \code{coef.table}. Values are (normalized) Euclidean distances.}
#' \item{\code{similarity.scores}}{A dataframe of similarity scores derive from \code{distance.matrix}. See Szmrecsanyi et al. (2019) for details.}
#' }
#'
#' @export
#'
#' @references Szmrecsanyi, Benedikt, Jason Grafmiller & Laura Rosseel. 2019. Variation-Based Distance and Similarity Modeling: A Case Study in World Englishes. \emph{Frontiers in Artificial Intelligence} 2. \url{https://doi.org/10.3389/frai.2019.00023}.
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
vadis_line2 <- function(mod_list, path = NULL, weight = 1, scale = c("abs", "mean", "minmax", "none"), overwrite = c("reload", "no",  "yes")){

  overwrite <- match.arg(overwrite)

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line2_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".rds")
    }

  if(overwrite == "reload" & file.exists(path)){
    # reload from existing file
    output_list <- readRDS(path)
  } else {
    output_list <- vector("list")
    raw_tab <- create_coef_table(mod_list) # call function to create varimp rankings
    output_list[[1]] <- raw_tab

    dist_mat <- dist(t(raw_tab[-1,]), method = "euclidean") # leave out the intercept

    if (match.arg(scale) == "abs"){
      # get the maximum reasonable distance
      dmy <- data.frame(a = sample(c(weight,-weight), size = nrow(raw_tab[-1,]), replace = T))
      dmy$b <- -dmy$a # exact opposite of a
      maxD <- max(dist(t(dmy), "euclidean"))
      out_dist <- dist_mat/maxD
    } else if (match.arg(scale) == "minmax"){
      out_dist <- minmax(dist_mat)
    } else if (match.arg(scale) == "mean"){
      out_dist <- (dist_mat - mean(dist_mat))/(max(dist_mat) - min(dist_mat))
    } else {
      out_dist <- dist_mat
    }

    # Now normalize all distances to the maximum reasonable distance
    weighted_dist <- as.matrix(out_dist)
    diag(weighted_dist) <- NA # remove diagonals before calculating means
    means <- colMeans(weighted_dist, na.rm = T)
    sim_tab <- data.frame(Similarity = 1 - means)
    rownames(sim_tab) <- names(mod_list)

    # save normalized distances to output
    output_list[[2]] <- out_dist
    output_list[[3]] <- as.data.frame(sim_tab)

    names(output_list) <- c("coef.table", "distance.matrix", "similarity.scores")
  }

  if(is.character(path)){
    if(overwrite == "yes"){
      saveRDS(output_list, file = path)
    } else if(overwrite == "no") {
      msg <- paste("File", path, "already exists. Overwrite (y/n)?: ")
      over <- readlines(prompt = msg)
      if(over == "y") {
        saveRDS(output_list, file = path)
      } else {
        new_path <- readlines(prompt = "Please enter new file path:")
        saveRDS(output_list, file = new_path)
      }
    }}

  return (output_list)
}
