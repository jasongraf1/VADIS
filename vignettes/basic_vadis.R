## ----setup, include = FALSE----------------------------------------------
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

## ----libs, comment=F, message=FALSE, error=FALSE-------------------------
library(tidyverse) # for data wrangling
library(lme4) # for regression models
library(party) # for random forests
library(phangorn) # for neighborNets

## ----eval = F------------------------------------------------------------
#  devtools::install_github("jasongraf1/VADIS")
#  library(VADIS)

## ----echo = F------------------------------------------------------------
library(VADIS)

## ------------------------------------------------------------------------
# call the dataset 
pv <- particle_verbs_short
names(pv)

## ----fig.height = 4, fig.width=6-----------------------------------------
ggplot(pv, aes(Variety, fill = Response)) +
  geom_bar(position = "dodge")

## ------------------------------------------------------------------------
# create list of dataframes
data_list <- split(pv, pv$Variety, drop = TRUE) # drop unused levels
names(data_list)

## ------------------------------------------------------------------------
f1 <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + 
  DirObjConcreteness + DirObjThematicity + DirectionalPP + PrimeType + 
  Semantics + Surprisal.P + Surprisal.V + Register

## ------------------------------------------------------------------------
glm_list <- vector("list") # empty list to store our models 
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # now standardize the model fixed effects inputs before fitting.
  d_std <- stand(d, cols = f1) # use the fitting function for convenience
  # fit the model
  glm_list[[i]] <- glm(f1, data = d_std, family = binomial, x = TRUE) # note the x = TRUE
}
names(glm_list) <- names(data_list) # add names to the list of models

## ------------------------------------------------------------------------
# update formula with by-verb and by-particle random intercepts 
f2 <- update(f1, .~ (1|Verb) + (1|Particle) + .)
f2

## ----eval = F------------------------------------------------------------
#  glmer_list <- vector("list")
#  for (i in seq_along(data_list)){
#    d <- data_list[[i]]
#    # standardize the model inputs, excluding the response and random effects
#    d_std <- stand(d, cols = f2) # use the fitting function for convenience
#    # fit the model
#    glmer_list[[i]] <- glmer(f2, data = d_std, family = binomial, # set optimizer controls to help convergence
#      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e7)))
#    rm(d, d_std) # remove datasets
#  }
#  names(glmer_list) <- names(data_list)

## ------------------------------------------------------------------------
summary_stats(glm_list) %>% 
  round(3)

## ------------------------------------------------------------------------
signif_line <- vadis_line1(glm_list, path = FALSE)

## ------------------------------------------------------------------------
signif_line$signif.table

## ------------------------------------------------------------------------
signif_line$distance.matrix %>% 
  round(3)

## ------------------------------------------------------------------------
signif_line$similarity.scores %>% 
  arrange(desc(Similarity)) # sort by similarity

## ----eval = F------------------------------------------------------------
#  write.csv(signif_line$signif.table,
#            file = "line1_significance_table.csv")
#  
#  write.csv(as.matrix(signif_line$distance.matrix),
#            file = "line1_distance_matrix.csv")
#  
#  write.csv(signif_line$similarity.scores,
#            file = "line1_similarity_scores.csv")

## ------------------------------------------------------------------------
coef_line <- vadis_line2(glm_list, path = FALSE)

## ------------------------------------------------------------------------
coef_line$coef.table %>% 
  round(3)

## ------------------------------------------------------------------------
coef_line$distance.matrix %>% 
  round(3)

## ------------------------------------------------------------------------
coef_line$similarity.scores %>% 
  arrange(desc(Similarity))

## ----crf_list------------------------------------------------------------
crf_list <- vector("list") # empty list to store our models
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # fit the random forest and add it to the list
  crf_list[[i]] <- cforest(f1, d, controls = cforest_unbiased(ntree = 500, mtry = 3))
}

## ----crf_list2, cache = F, eval = F--------------------------------------
#  crf_list <- lapply(data_list,
#    FUN = function(d) cforest(f1, d, controls = cforest_unbiased(ntree = 500, mtry = 3)))

## ------------------------------------------------------------------------
summary_stats(crf_list, data_list, response = "Response") %>% 
  round(3)

## ----line3, cache=F------------------------------------------------------
varimp_line <- vadis_line3(crf_list, path = FALSE, conditional = FALSE)

## ------------------------------------------------------------------------
varimp_line$distance.matrix %>% 
  round(3)

## ------------------------------------------------------------------------
varimp_line$similarity.scores %>% 
  arrange(desc(Similarity))

## ----echo=F, eval = T----------------------------------------------------
d <- data.frame(
  a = rep(c(-1,1), each = 5),
  b = rep(c(1,-1), each = 5))
d

## ------------------------------------------------------------------------
mean_sims <- data.frame(
  line1 = signif_line$similarity.scores[,2], # get only the values in the 2nd column
  line2 = coef_line$similarity.scores[,2],
  line3 = varimp_line$similarity.scores[,2]
)
mean_sims$mean <- apply(mean_sims, 1, mean)
rownames(mean_sims) <- names(data_list)
round(mean_sims, 3)

## ------------------------------------------------------------------------
mean(mean_sims$mean)

## ------------------------------------------------------------------------
fused_dist <- analogue::fuse(signif_line$distance.matrix, 
                             coef_line$distance.matrix, 
                             varimp_line$distance.matrix)
round(fused_dist, 3)

## ----hclustplot, fig.height=6, fig.width=7-------------------------------
# Use the 2nd line of evidence
line2_clust <- hclust(coef_line$distance.matrix, method = "ward.D2")
plot(line2_clust, main = "Hierarchical clustering of line 2 distances")

## ----fig.height=6, fig.width=7-------------------------------------------
hclust(fused_dist, method = "ward.D2") %>% 
  plot(main = "Hierarchical clustering of fused distances")

## ----fig.height=6, fig.width=7-------------------------------------------
cluster::diana(coef_line$distance.matrix) %>% 
  cluster::pltree(main = "Divisive clustering of line 2 distances")

## ----fig.height=6, fig.width=7-------------------------------------------
ape::nj(coef_line$distance.matrix) %>% 
  plot(type = "u", main = "unrooted clustering of line 2 distances")

## ----fig.height=6, fig.width=7-------------------------------------------
ape::nj(fused_dist) %>% 
  plot(type = "u", main = "unrooted clustering of fused distances")

## ------------------------------------------------------------------------
line2_mds <- cmdscale(coef_line$distance.matrix, k = 3, eig = T) 

## ----mdsplot, fig.height=6, fig.width=7----------------------------------
line2_mds[[1]] %>%
  as.data.frame() %>% 
  mutate(genres = rownames(.)) %>% 
  ggplot(aes(V1, V2, label = genres)) +
  geom_point() +
  geom_text(nudge_y = .01, size = 4)

## ----fig.height=6, fig.width=7-------------------------------------------
dd <- line2_mds[[1]] %>%
  as.data.frame() 
library(scatterplot3d)
with(dd, {
  scttr <- scatterplot3d(x = V1, y = V2, z = V3, type = "h", pch = 18)
  scttr_coords <- scttr$xyz.convert(V1, V2, V3)
  text(scttr_coords$x, scttr_coords$y, labels = rownames(dd), pos = 3)
  })

## ----fig.height=6, fig.width=7-------------------------------------------
library(plotly)
dd %>% 
  mutate(Variety = rownames(.)) %>% 
  plot_ly() %>%
  add_trace(x = ~V1, y = ~V2, z = ~V3,
            type = "scatter3d", inherit = F,
            marker = list(size = 4),
            mode = "markers") %>%
  add_text(x = ~V1, y = ~V2, z = ~V3,
           text = ~ Variety,
           type = "scatter3d",
           mode = "markers",
           showlegend = FALSE)

## ------------------------------------------------------------------------
fused_mds <- cmdscale(fused_dist, k = 3, eig = T)

## ----fig.height=6, fig.width=7-------------------------------------------
fused_mds[[1]] %>% # extract the coordinates
  as.data.frame() %>% 
  mutate(Variety = rownames(.)) %>% 
  plot_ly() %>%
  add_trace(x = ~V1, y = ~V2, z = ~V3,
            type = "scatter3d", inherit = F,
            marker = list(size = 4),
            mode = "markers") %>%
  add_text(x = ~V1, y = ~V2, z = ~V3,
           text = ~ Variety,
           type = "scatter3d",
           mode = "markers",
           showlegend = FALSE)

## ----NNetplot, fig.height=6, fig.width=7---------------------------------
line2_NNet <- phangorn::neighborNet(coef_line$distance.matrix)
plot(line2_NNet, "2D")

## ----NNetplot2, fig.height=6, fig.width=7--------------------------------
phangorn::neighborNet(fused_dist) %>% 
  plot("2D")

