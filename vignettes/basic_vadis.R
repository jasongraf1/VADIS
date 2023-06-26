## ----setup, include = FALSE---------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  error = FALSE,
  message = FALSE,
  comment = "#>"
)
knitr::opts_knit$set(
  root.dir = normalizePath('../'), # set working directory as root
  cache.path = "vignettes/basic_vadis_cache/") 

## ----libs, comment=F, message=FALSE, error=FALSE------------------------------
library(tidyverse) # for data wrangling
library(lme4) # for regression models
library(ranger) # for random forests
library(permimp) # for conditional permutation importance
library(phangorn) # for neighborNets

## ----eval = F-----------------------------------------------------------------
#  devtools::install_github("jasongraf1/VADIS")

## -----------------------------------------------------------------------------
library(VADIS)

## -----------------------------------------------------------------------------
# call the dataset 
pv <- particle_verbs_short
names(pv)

## ----fig.height = 4, fig.width=6----------------------------------------------
ggplot(pv, aes(Variety, fill = Response)) +
  geom_bar(position = "dodge")

## -----------------------------------------------------------------------------
# create list of dataframes
data_list <- split(pv, pv$Variety, drop = TRUE) # drop unused levels
names(data_list)

## -----------------------------------------------------------------------------
f1 <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + 
  DirObjConcreteness + DirObjThematicity + DirectionalPP + PrimeType + 
  Semantics + Surprisal.P + Surprisal.V + Register

## -----------------------------------------------------------------------------
glm_list <- vector("list") # empty list to store our models 
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # now standardize the model fixed effects inputs before fitting.
  d_std <- stand(d, cols = f1) # use the fitting function for convenience
  # fit the model
  glm_list[[i]] <- glm(f1, data = d_std, family = binomial, x = TRUE) # note the x = TRUE
}
names(glm_list) <- names(data_list) # add names to the list of models

## -----------------------------------------------------------------------------
# update formula with by-verb and by-particle random intercepts 
f2 <- update(f1, .~ (1|Verb) + (1|Particle) + .)
f2

## ----eval = F-----------------------------------------------------------------
#  glmer_list <- vector("list")
#  for (i in seq_along(data_list)){
#    d <- data_list[[i]]
#    # standardize the model inputs, excluding the response and random effects
#    d_std <- stand(d, cols = f2) # use the fitting function for convenience
#    # fit the model
#    glmer_list[[i]] <- glmer(f2, data = d_std, family = binomial,
#      # set optimizer controls to help convergence
#      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e7)))
#    rm(d, d_std) # remove datasets
#  }
#  names(glmer_list) <- names(data_list)

## ----warning = F--------------------------------------------------------------
summary_stats(glm_list, data_list) %>% 
  round(3)

## -----------------------------------------------------------------------------
signif_line <- vadis_line1(glm_list, path = FALSE)

## -----------------------------------------------------------------------------
signif_line$signif.table

## -----------------------------------------------------------------------------
signif_line$distance.matrix %>% 
  round(3)

## -----------------------------------------------------------------------------
signif_line$similarity.scores %>% 
  arrange(desc(Similarity)) # sort by similarity

## ----eval = F-----------------------------------------------------------------
#  write.csv(signif_line$signif.table,
#            file = "line1_significance_table.csv")
#  
#  write.csv(as.matrix(signif_line$distance.matrix),
#            file = "line1_distance_matrix.csv")
#  
#  write.csv(signif_line$similarity.scores,
#            file = "line1_similarity_scores.csv")

## -----------------------------------------------------------------------------
coef_line <- vadis_line2(glm_list, path = FALSE)

## -----------------------------------------------------------------------------
coef_line$coef.table %>% 
  round(3)

## -----------------------------------------------------------------------------
coef_line$distance.matrix %>% 
  round(3)

## -----------------------------------------------------------------------------
coef_line$similarity.scores %>% 
  arrange(desc(Similarity))

## ----echo=F, eval = T---------------------------------------------------------
d <- data.frame(
  A = rep(c(-1,1), each = 5),
  B = rep(c(1,-1), each = 5),
  row.names = paste0("constraint.", 1:10))
d

## ----eval = F-----------------------------------------------------------------
#  vadis_line2(glm_list, weight = 2, path = FALSE)$distance.matrix %>%
#    round(3)

## ----eval = F-----------------------------------------------------------------
#  library(tuneRanger)
#  library(mlr)
#  
#  tune_df <- data.frame(matrix(NA, ncol = 5, nrow = 9))
#  names(tune_df) <- c("mtry", "min.node.size", "sample.fraction", "auc", "exec.time")
#  
#  for (i in seq_along(data_list)){
#    d <- data_list[[i]][, all.vars(f1)]
#    pv_task <- makeClassifTask(data = d, target = "Response")
#  
#    # Tuning process (takes around 1 minute); Tuning measure is the Area Under the Curve
#    result <- tuneRanger(pv_task, measure = list(auc), num.trees = 1000,
#                      num.threads = 4, iters = 80, show.info = F)
#  
#    tune_df[i,] <- result$recommended.pars
#  }
#  rownames(tune_df) <- names(data_list)

## ----echo = F-----------------------------------------------------------------
tune_df <- read.csv("data/tune_df.csv")

## -----------------------------------------------------------------------------
library(ranger)
rf_list <- vector("list")
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # fit the random forest and add it to the list
  rf_list[[i]] <- ranger(
    f1, 
    data = d,
    seed = 1234,
    num.trees = 1000,
    mtry = tune_df[i, "mtry"],
    min.node.size = tune_df[i, "min.node.size"],
    sample.fraction = tune_df[i, "sample.fraction"],
    probability = TRUE,
    importance = "permutation"
    )
}
names(rf_list) <- names(data_list)

## -----------------------------------------------------------------------------
summary_stats(rf_list, data_list, response = "Response") %>% 
  round(3)

