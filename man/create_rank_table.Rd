% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_rank_table.R
\name{create_rank_table}
\alias{create_rank_table}
\title{Create Table of Variable Importance Rankings for VADIS analysis}
\usage{
create_rank_table(mod_list, conditional = TRUE)
}
\arguments{
\item{mod_list}{A list of random forest model objects. Currently supports objects of class \code{\link[party]{RandomForest-class}}, \code{\link[ranger]{ranger}}, and \code{\link[randomForest]{randomForest}}}

\item{conditional}{logical. Should conditional (default) or unconditional permutation variable importance be computed. Only applies to \code{RandomForest-class} models from the \code{\link[party]{party}} package.}

\item{path}{Path in which to save the output (as \code{.csv} file). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.}
}
\value{
A dataframe
}
\description{
Create Table of Variable Importance Rankings for VADIS analysis
}
\details{
The function loops through a list of random forest objects, extracts the variable importance estimates, and compiles them in a single dataframe. For \code{\link[party]{RandomForest-class}} models, function uses the \code{\link[permimp]{permimp}} function (Debeer & Strobl 2020) with the default threshold (0.95) and \code{AUC= TRUE} (Janitza et al. 2013).
For forests fit with \code{\link[ranger]{ranger}} or \code{\link[randomForest]{randomForest}}, the \code{importance} argument must be specified.
#'
}
\examples{
\dontrun{
fmla <- Type ~ PossrAnimacyBin + PossrWordC + PossmWordC + FinalSibilant +
  TypeTokenRatio + ProtoSemanticRelation + PossrExpType

rf_fnc <- function(x) ranger::ranger(fmla, data = x, importance = "permutation")

rf_list <- lapply(data_list, rf_func)

create_rank_table(rf_list, path = FALSE)
}
}
\references{
Debeer, Dries & Carolin Strobl. 2020. Conditional permutation importance revisited. *BMC Bioinformatics* 21(1). 307. https://doi.org/10.1186/s12859-020-03622-2.

Janitza, Silke, Carolin Strobl & Anne-Laure Boulesteix. 2013. An AUC-based permutation variable importance measure for random forests. *BMC Bioinformatics* 14(1). 119. https://doi.org/10.1186/1471-2105-14-119.
}
\author{
Jason Grafmiller
}
