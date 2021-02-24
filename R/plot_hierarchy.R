#' Plot hierarchy of a constraint's levels across multiple models
#'
#' @param focal.predictors
#' @param mod_list
#' @param data
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
plot_hierarchy <- function(focal.predictors = NULL, mod_list,
                           data = data_list, ncol = 3){
  if(is.null(focal.predictors)) stop("No predictor names added...")
  levs <- levels(data_list[[i]][, focal.predictors])
  partials_df <- data.frame()
  for(i in seq_along(mod_list)){
    m <- mod_list[[i]]
    assign(as.character(m@call[[3]]), stand(data_list[[i]], formula(m)))
    partials <- as.data.frame(ggeffects::ggeffect(m, focal.predictors,
                                                  transformation = NULL))
    partials$group <- names(data_list)[i]
    partials$x <- levs
    partials$x <- factor(partials$x)
    names(partials)[1] <- focal.predictors
    partials_df <- rbind(partials_df, partials)
  }
  p <- ggplot(partials_df, aes_string(focal.predictors, "predicted")) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
    geom_point() +
    facet_wrap(~ group, ncol = 3)
  if(any(nchar(levs) > 10)) {
    p <- p + scale_x_discrete(labels = abbreviate(levs))
  }
  p
}
