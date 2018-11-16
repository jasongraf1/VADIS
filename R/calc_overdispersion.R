calc_overdispersion <- function(mod) {
  ## Diagnose overdispersion in a model's response variable
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  # from http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
  rdf <- df.residual(mod)
  rp <- residuals(mod, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  data.frame(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
