#' Create Table of Variable Importance Rankings for VADIS analysis
#'
#' @param mod_list A list of random forest model objects. Currently supports objects of class \code{\link[party]{RandomForest-class}}, \code{\link[ranger]{ranger}}, and \code{\link[randomForest]{randomForest}}
#' @param path Path in which to save the output (as \code{.csv} file). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.
#' @param conditional logical. Should unconditional (default) or conditional permutation variable importance be computed. Only applies to \code{RandomForest-class} models from the \code{\link[party]{party}} package.
#'
#' @author Jason Grafmiller
#'
#' @details The function loops through a list of random forest objects, extracts the variable importance estimates, and compiles them in a single dataframe.
#' For forests fit with \code{\link[ranger]{ranger}} or \code{\link[randomForest]{randomForest}}, the \code{importance} argument must be specified.
#' #'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fmla <- Type ~ PossrAnimacyBin + PossrWordC + PossmWordC + FinalSibilant +
#'   TypeTokenRatio + ProtoSemanticRelation + PossrExpType
#'
#' rf_fnc <- function(x) ranger::ranger(fmla, data = x, importance = "permutation")
#'
#' rf_list <- lapply(data_list, rf_func)
#'
#' create_rank_table(rf_list, path = FALSE)
#' }
create_rank_table <- function(mod_list, conditional = FALSE) {
  # identify the class of models
  type <- class(mod_list[[1]])
  if (type[1] == "ranger"){
    varimp_tab <- as.data.frame(do.call("cbind", lapply(crf_list, FUN = function(m) ranger::importance(m))))
  } else if (type[1] == "RandomForest"){
    if (conditional){
      cat(paste("Computing varimpAUC() for", length(mod_list), "models. This may take some time...\nIf it takes too long, consider setting conditional = FALSE."))
    } else cat(paste("Computing varimpAUC() for", length(mod_list), "models. This may take some time..."))
    varimp_list <- lapply(mod_list, FUN = function(m) party::varimpAUC(m, conditional = conditional))
    varimp_tab <- data.frame(sapply(varimp_list, c))
    names(varimp_tab) <- names(mod_list)
  } else if (type == "randomForest"){
    varimp_list <- lapply(mod_list, FUN = function(m) randomForest::importance(m))
    varimp_tab <- data.frame(sapply(varimp_list, c))
    names(varimp_tab) <- names(mod_list)
  } else stop("I don't recognize this class of random forest model...")
  return (varimp_tab)
}
