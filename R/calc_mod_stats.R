calc_mod_stats <- function(mod, df = NULL, response = NULL){
  mclass <- class(mod)[1]
  if (mclass %in% c("glm", "merMod")){
    fits <- fitted(mod)
    preds <- ifelse(fits > .5, 1, 0)
    # get the response column and the AIC for the model
    if (mclass == "glm"){
      y <- mod$y # vector of responses
      aic <- mod$aic
    } else if (mclass == "merMod") {
      y <- lme4::getME(fit, "y") # vector of responses
      aic <- extractAIC(mod)
    }
    # calculate C index and Dxy
    mean.rank <- mean(rank(fits)[y == 1])
    n <- length(y)
    n1 <- sum(y == 1)
    c.index <- (mean.rank - (n1 + 1)/2)/(n - n1)
    # Dxy <- 2 * (c.index - .05)
    maxVIF <- max(calc_VIF(mod))
    kappa <- calc_kappa(mod)
    overdisp <- calc_overdispersion(mod)$p
    # put the output into a vector
    output <- c(
      N = as.integer(n),
      baseline = max(table(y)/length(y)),
      predicted.corr = mean(preds == y),
      C = c.index,
      AIC = aic,
      Max.VIF = maxVIF,
      kappa = kappa,
      overdisp.p = round(overdisp, 3))
  } else if (mclass %in% c("ranger", "RandomForest", "randomForest")){
    if (is.null(response)) {
      stop("You must enter a column name for the response variable")
    } else if (is.null(df)) stop("You must enter a list of dataframes")
    resp <- df[, response]
    y <- as.numeric(resp) - 1
    if (mclass == "ranger"){
      preds <- mod$predictions
      fits <- as.numeric(preds) - 1
    } else if (mclass == "randomForest"){
      preds <- predict(mod, df, type = "response")
      fits <- as.numeric(preds) - 1
    } else if(mclass == "RandomForest"){
      trp <- treeresponse(mod)
      fits <- sapply(trp, FUN = function(x) x[2])
      preds <- ifelse(fits > .5, 1, 0)
    }
    # put the output into a vector
    # calculate C index and Dxy
    mean.rank <- mean(rank(fits)[y == 1])
    n <- length(y)
    n1 <- sum(y == 1)
    c.index <- (mean.rank - (n1 + 1)/2)/(n - n1)
    output <- data.frame(
      N = as.integer(n),
      baseline = max(table(y)/length(y)),
      predicted.corr = mean(preds == y),
      C = c.index)
  } else stop("I don't recognize this model class...")
  return(output)
}