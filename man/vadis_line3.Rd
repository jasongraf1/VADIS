% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vadis_line3.R
\name{vadis_line3}
\alias{vadis_line3}
\title{Calculate the third line of evidence for the VADIS method}
\usage{
vadis_line3(
  mod_object,
  path = NULL,
  conditional = TRUE,
  overwrite = c("no", "yes", "reload"),
  verbose = FALSE
)
}
\arguments{
\item{mod_object}{Either a list of random forest model objects, or a dataframe of variable importance scores, where rows represent predictors in the models and columns represent the varieties to compare.}

\item{path}{Path in which to save the output as an R data file (\code{.rds}). If \code{NULL}, defaults to the current working directory. Set \code{path = FALSE} if you do not wish to save to file.}

\item{conditional}{logical. Should conditional (default) or unconditional permutation variable importance be computed. Only applies to \code{RandomForest-class} models from the \code{\link[party]{party}} package.}

\item{overwrite}{Should the function overwrite data to location in \code{path}? Default is \code{'no'}, which will run the analysis if no file exists. If file in \code{path} exists, user with be prompted to set new path or allow file to be overwritten. Set to \code{'yes'} to automatically overwrite existing file, and \code{'reload'} to automatically reload existing file.}

\item{verbose}{Should messages be printed? Default is \code{FALSE}}
}
\value{
A \code{list} of length 4.
\describe{
\item{\code{varimp.table}}{A dataframe of \emph{P} predictors by \emph{M} random forest models, containing the variable importance scores obtained for each predictor in the models. }
\item{\code{rank.table}}{A dataframe of \emph{P} predictors by \emph{M} random forest models, containing the rankings of predictors in each model by variable importance score in \code{varimp.table}.}
\item{\code{distance.matrix}}{An \emph{M} by \emph{M} distance matrix of class \code{dist}, derived from \code{rank.table}. Values are calculated as 1 - the Spearman rank correlation \rho.}
\item{\code{similarity.scores}}{A dataframe of similarity scores derive from \code{distance.matrix}. See Szmrecsanyi et al. (2019) for details.}
}
}
\description{
Calculate the third line of evidence for the VADIS method
}
\details{
The function loops through a list of model objects, extracts the coefficient estimates, and compiles them in a single dataframe.
}
\examples{
\dontrun{
data_list <- split(particle_verbs_short, particle_verbs_short$Variety, drop = TRUE)

fmla <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + DirObjConcreteness + DirObjThematicity + DirectionalPP + PrimeType + Semantics + Surprisal.P + Surprisal.V + Register

rf_func <- function(x) ranger(fmla, data = x, importance = "permutation")

rf_list <- lapply(data_list, rf_func)
names(rf_list) <- names(data_list)

line3 <- vadis_line3(rf_list, path = FALSE)
}
}
\references{
Szmrecsanyi, Benedikt, Jason Grafmiller & Laura Rosseel. 2019. Variation-Based Distance and Similarity Modeling: A Case Study in World Englishes. \emph{Frontiers in Artificial Intelligence} 2. \url{https://doi.org/10.3389/frai.2019.00023}.
}
\author{
Jason Grafmiller
}
