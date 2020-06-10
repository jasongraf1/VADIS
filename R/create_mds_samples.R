#' Create dataframe of coordinates for multidmensional scaling of posterior samples
#'
#' @param mod_list list of regression model objects of class \code{brmsfit}.
#' @param n_samples integer indicating number of samples to pull
#' @param k = the maximum dimension of the space which the data are to be represented in the MDS; must be in ${1, 2, …, n-1}$ (see \code{\link[stats]{cmdscale}}).
#' @param vadis_line list containing the output of \code{vadis_line2}. If \code{NULL} (default), this will be run automatically
#' @param path filepath to which to save the output as an .csv file. If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#'
#'@author Jason Grafmiller
#'
#' @details The function creates a dataframe containing MDS coordinates of distance matrices derived from posterior samples of coefficient estimates. Currently only works with \code{brmsfit} objects.
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Coming soon...
#' }
create_mds_samples <- function(mod_list, n_samples = 200L, k = 3, vadis_line = NULL, path = NULL){

  if(is.null(vadis_line)){
    ## Get the second line if it doesn't exist
    vadis_line <- vadis_line2(mod_list)
  }

  orig_dist <- dist(t(vadis_line$coef.table[-1,]), method = "euclidean")
  orig_mds <- cmdscale(orig_dist, k = k, eig = T)
  orig_mds_df <- as.data.frame(orig_mds[[1]]) %>%
    rename(x = "V1", y = "V2", z = "V3")

  sample_list <- get_posterior_samples(mod_list, n = n_samples)

  mds_list <- vector("list")
  ncoef <- nrow(sample_list[[1]]) ## number of coefficients in the model(s)
  nvar <- length(sample_list) ## number of varieties
  coefs <- rownames(sample_list[[1]])
  vars <- names(sample_list) ## names of varieties

  ## Loop through list of samples
  for(i in 1:ncol(sample_list[[1]])){
    cur_df <- data.frame(matrix(ncol = ncoef, nrow = nvar))
    for (j in seq_along(sample_list)){
      cur_df[j, ] <- sample_list[[j]][,i]
    }
    rownames(cur_df) <- vars
    names(cur_df) <- coefs
    ## Create distance matrix
    cur_dist <- dist(cur_df, "euclidean")

    ## Get the maximum reasonable distance
    # dmy <- data.frame(a = sample(c(1,-1), size = ncol(cur_df), replace = T))
    # dmy$b <- -dmy$a # exact opposite of a
    # maxD <- max(dist(t(dmy), "euclidean"))
    #
    # ## Create weighted distance matrix
    # cur_dist_wt <- cur_dist/maxD

    cur_mds <- cmdscale(cur_dist, k = k)
    colnames(cur_mds) <- letters[(27-k):26]
    cur_mds = as.data.frame(cur_mds)
    ## align
    for (dm in seq_len(k)){
      if(cor(orig_mds_df[, dm], cur_mds[, dm]) < 0) {
        cur_mds[, dm] <- cur_mds[, dm]*-1
      }}
    mds_list[[i]] <- cur_mds
  }
  names(mds_list) <- colnames(sample_list[[1]])

  mds_variety_df <- bind_rows(mds_list, .id = "run") %>%
    rownames_to_column() %>%
    mutate(
      variety = gsub("\\.\\..*", "", rowname) %>%
        as.factor()
    )

  if (is.null(path)) {
    path <- paste0(getwd(), "/vadis_line2_output_", format(Sys.time(), "%Y-%b-%d_%H-%M"), ".csv")
    write_csv(output_list, file = path)
    } else if (is.character(path)) {
    write_csv(output_list, file = path)
  }
  return(mds_variety_df)
}