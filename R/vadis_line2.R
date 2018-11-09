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
#' lm_fnc <- function(x) lm(Sepal.Length ~ Petal.Length + Petal.Width + Sepal.width, data = x)
#' rm_list <- fit.vadis.RM(iris, split.by = "Species", fit.func = lm_fnc,
#'   path = FALSE)
#' summary(rm_list[[1]])
#'
#' line2 <- calc_line2(rm_list, path = FALSE)
#' }
vadis_line2 <- function(mod_list, path = NULL){
  output_list <- vector("list")
  raw_tab <- create_coef_table(mod_list) # call function to create varimp rankings
  output_list[[1]] <- raw_tab

  dist_mat <- dist(t(raw_tab[-1,]), method = "euclidean") # leave out the intercept

  # get the maximum reasonable distance
  dmy <- data.frame(a = sample(c(1,-1), size = nrow(raw_tab), replace = T))
  dmy$b <- -dmy$a # exact opposite of a
  maxD <- max(dist(t(dmy), "euclidean"))

  # save normalized distances to output
  output_list[[2]] <- dist_mat/maxD

  # Now normalize all distances to the maximum reasonable distance
  sim_tab <- dist_mat %>%
    as.matrix() %>%
    as.data.frame() %>%
    reshape2::melt(id.vars = NULL) %>%
    mutate(weighted = value/maxD) %>% # weight distances by maxD
    group_by(variable) %>%
    dplyr::filter(value > 0) %>%      # ignore distances to self
    summarise(Similarity = 1 - mean(weighted)) %>%
    arrange(desc(Similarity))

  output_list[[3]] <- as.data.frame(sim_tab)

  names(output_list) <- c("coef.table", "distance.matrix", "similarity.coefs")

  if (path == FALSE) {
    return (output_list)
  } else if (is.null(path)) {
    path <- paste0(getwd(), "/", "vadis_line2_output", format(Sys.time(), "%Y-%b-%d_%H.%M"), ".rds")
    saveRDS(output_list, file = path)
    return (output_list)
  } else {
    saveRDS(output_list, file = path)
    return (output_list)
  }
}
