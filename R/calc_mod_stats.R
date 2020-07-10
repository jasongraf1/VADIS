calc_mod_stats <- function(mod, data = NULL, response = NULL){
  mclass <- class(mod)[1]
  if (mclass %in% c("glm", "glmerMod")){
    fits <- fitted(mod)
    preds <- ifelse(fits > .5, 1, 0)
    # get the response column and the AIC for the model
    if (mclass == "glm"){
      y <- mod$y # vector of responses
      aic <- mod$aic
    } else if (mclass == "glmerMod") {
      y <- lme4::getME(mod, "y") # vector of responses
      aic <- extractAIC(mod)[2]
    }
    # calculate Brier, C index and Dxy
    brier_score <- mean((fits - y)^2)
    mean.rank <- mean(rank(fits)[y == 1])
    n <- length(y)
    n1 <- sum(y == 1)
    c.index <- (mean.rank - (n1 + 1)/2)/(n - n1)
    # Dxy <- 2 * (c.index - .05)
    maxVIF <- max(calc_VIF(mod))
    if (class(mod)[1] %in% c("glm", "lm") & length(mod$x) == 0){
      kappa <- NA
    } else kappa <- calc_kappa(mod)
    hoslem <- hosmerlem_test(mod)$p.value
    # put the output into a vector
    output <- c(
      N = as.integer(n),
      baseline = max(table(y)/length(y)),
      predicted.corr = mean(preds == y),
      Brier = brier_score,
      C = c.index,
      AIC = aic,
      Max.VIF = maxVIF,
      kappa = kappa,
      hoslem.p = round(hoslem, 3))
    resp <- data[, response]
    msg <- paste("Predictions are for", levels(resp)[2])
  } else if (mclass == "brmsfit") {
    resp <- data[, response]
    y <- as.numeric(resp) - 1
    fits <- fitted(mod)
    preds <- ifelse(fits$Estimate > .5, 1, 0)
    brier_score <- mean((fits - y)^2)
    mean.rank <- mean(rank(fits)[y == 1])
    n <- length(y)
    n1 <- sum(y == 1)
    c.index <- (mean.rank - (n1 + 1)/2)/(n - n1)
    kappa <- calc_kappa(mod)
    # get the loo estimates
    loo_estimates <- mod$criteria$loo$estimates

    # put the output into a vector
    output <- c(
      N = as.integer(n),
      baseline = max(table(y)/length(y)),
      predicted.corr = mean(preds == y),
      Brier = brier_score,
      C = c.index,
      WAIC = aic,
      elpd_loo = loo_estimates[1, 1],
      p_loo = loo_estimates[2, 1],
      looic = loo_estimates[3, 1],
      kappa = kappa
    )

  } else if (mclass %in% c("ranger", "RandomForest", "randomForest")){
    if (is.null(response)) {
      stop("You must enter a column name for the response variable")
    } else if (is.null(data)) stop("You must enter a list of dataframes")
    resp <- data[, response]
    y <- as.numeric(resp) - 1
    if (mclass == "ranger"){
      ## ranger allows predicted responses or probabilities
      preds <- mod$predictions
      if(is.factor(preds)){
        ## if `probability = FALSE`, predictions are factor of the response
        ## values
        fits <- as.numeric(preds) - 1
        predicted_class <- levels(preds)[2]
        pred_correct <- 1 - mod$prediction.error
      } else {
        ## if `probability = TRUE`, predictions are matrix of the predicted
        ## probabilities for each response class
        fits <- preds[, 2] ## second column in matrix
        predicted_class <- colnames(preds)[2]
        msg <- paste("Predictions are for", predicted_class)
        preds <- ifelse(fits > .5, 1, 0)
        pred_correct <- sum(diag(table(preds, y)))/length(y)
        brier_score <- mod$prediction.error
      }

    } else if (mclass == "randomForest"){
      preds <- mod$predicted
      fits <- as.numeric(preds) - 1
      brier_score <- NA
    } else if(mclass == "RandomForest"){
      trp <- treeresponse(mod)
      predicted_class <- colnames(trp)[2]
      fits <- sapply(trp, FUN = function(x) x[2])
      preds <- ifelse(fits > .5, 1, 0)
      pred_correct = sum(diag(table(preds, y)))/length(y)
      msg <- paste("Predictions are for", predicted_class)
      brier_score <- mean((fits - y)^2)
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
      predicted.corr = pred_correct,
      Brier = brier_score,
      C = c.index)
  } else if (mclass == "brmsfit") {

  } else stop(paste("I don't recognize this model class:", mclass))
  cat(msg, sep = "\n")
  return(output)
}
