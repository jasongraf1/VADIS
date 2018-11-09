#' Calculate the maximum distances for a set of possible constraints
#'
#' @param n vector of integers representing different numbers of constraints
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' calc_max_distances(6:20)
#' }
calc_max_distances <- function(n = 6:20){
  df <- data.frame(n.preds = rep(0, length(n)),
    max.distance = rep(0, length(n)))
  for(i in 1:length(n)){
    size <- n[i]
    dmy <- data.frame(a = sample(c(1,-1), size = size, replace = T))
    dmy$b <- -dmy$a # exact opposite of a
    maxD <- max(dist(t(dmy), "euclidean"))
    df[i,] <- c(size, maxD)
  }
  return(df)
  }