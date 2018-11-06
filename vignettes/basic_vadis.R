## ----setup, include = FALSE----------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = normalizePath('../')) # set working directory as root

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

## ----fig.height = 5, fig.width=6-----------------------------------------
ggplot(pv, aes(Variety, fill = Response)) +
  geom_bar(position = "dodge")

## ------------------------------------------------------------------------
# create list of dataframes
data_list <- split(pv, pv$Variety, drop = TRUE) # drop unused levels
names(data_list)

## ------------------------------------------------------------------------
f1 <- Response ~ DirObjWordLength + DirObjDefiniteness + DirObjGivenness + DirObjConcreteness + 
  DirObjThematicity + DirectionalPP + PrimeType + Semantics + Surprisal.P + Surprisal.V + Register

## ------------------------------------------------------------------------
glm_list <- vector("list")
for (i in seq_along(data_list)){
  d <- data_list[[i]]
  # now we'll standardize the model inputs (excluding the response) before fitting 
  d[all.vars(f1)[-1]] <- lapply(d[all.vars(f1)[-1]], FUN = stand)  
  glm_list[[i]] <- glm(f1, data = d, family = binomial)
}
names(glm_list) <- names(data_list)

## ----eval = F------------------------------------------------------------
#  # update formula with by-verb and by-particle random intercepts
#  f2 <- update(f1, .~ (1|Verb) + (1|Particle) + .)
#  
#  glmer_list <- vector("list")
#  for (i in seq_along(data_list)){
#    d <- data_list[[i]]
#    # standardize the model inputs (excluding the response and random effects) before fitting
#    d[all.vars(f2)[-c(1:3)]] <- lapply(d[all.vars(f2)[-c(1:3)]], FUN = stand)
#    # set optimizer controls to help convergence
#    glmer_list[[i]] <- glmer(f2, data = d, family = binomial,
#      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e7)))
#  }
#  names(glmer_list) <- names(data_list)

## ------------------------------------------------------------------------
signif_line <- vadis_line1(glm_list, path = FALSE)

## ------------------------------------------------------------------------
signif_line$signif.table

## ------------------------------------------------------------------------
signif_line$distance.matrix

## ------------------------------------------------------------------------
signif_line$similarity.coefs

## ------------------------------------------------------------------------
coef_line <- vadis_line2(glm_list, path = FALSE)

## ------------------------------------------------------------------------
coef_line$coef.table %>% 
  round(3)

## ------------------------------------------------------------------------
coef_line$distance.matrix %>% 
  round(3)

## ------------------------------------------------------------------------
coef_line$similarity.coefs

## ------------------------------------------------------------------------
crf_func <- function(d) {
  cforest(f1, d, controls = cforest_unbiased(ntree = 500, mtry = 3))
}

## ------------------------------------------------------------------------
crf_list <- lapply(data_list, FUN = crf_func)

## ------------------------------------------------------------------------
varimp_line <- vadis_line3(crf_list, path = FALSE, conditional = FALSE)

## ------------------------------------------------------------------------
varimp_line$distance.matrix %>% 
  round(3)

## ------------------------------------------------------------------------
varimp_line$similarity.coefs

## ----hclustplot, fig.height=6, fig.width=7-------------------------------
line2_clust <- hclust(coef_line$distance.matrix, method = "ward.D2")
plot(line2_clust)

## ----fig.height=6, fig.width=7-------------------------------------------
cluster::diana(coef_line$distance.matrix) %>% 
  cluster::pltree()

## ----fig.height=6, fig.width=7-------------------------------------------
ape::nj(coef_line$distance.matrix) %>% 
  plot(type = "u")

## ------------------------------------------------------------------------
line2_mds <- cmdscale(coef_line$distance.matrix, k = 3, eig = T) 

## ----mdsplot, fig.height=6, fig.width=7----------------------------------
line2_mds[[1]] %>%
  as.data.frame() %>% 
  mutate(genres = rownames(.)) %>% 
  ggplot(aes(V1, V2, label = genres)) +
  geom_point() +
  geom_text(nudge_y = .05, size = 4)

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

## ----NNetplot, fig.height=6, fig.width=7---------------------------------
line2_NNet <- phangorn::neighborNet(coef_line$distance.matrix)
plot(line2_NNet, "2D")

