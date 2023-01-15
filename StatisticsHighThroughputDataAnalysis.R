---
title: "SpatialR"
output: html_document
date: "2023-01-14"
source: http://www.sthda.com/french/
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Articles - Principal Component Methods in R: Practical Guide
```{r, echo=FALSE}
# Installation
#install.packages("FactoMineR")
# Chargement
library("FactoMineR")
#install.packages("factoextra")
library("factoextra")

#---------------------------------
# Case of continuous variables
#---------------------------------
# Visualize individuals and color in groups

# 1. ACP 
res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)
# 2. HCPC
res.hcpc <- HCPC(res.pca, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
          )

#---------------------------------
# 3D graph combining hierarchical classification and factor plan.

fviz_cluster(res.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
             )

#---------------------------------
# Content of the function result:HCPC()
# data.clust: Original data with an additional column called containing the groups.clust
# desc.var: the variables describing the groups
# desc.ind: the most typical individuals of each group
# desc.axes: the axes describing the groups

# Original data with column:class

# Principal components + tree
plot(res.hcpc, choice = "3D.map")

head(res.hcpc$data.clust, 10)
res.hcpc$desc.var$quanti
res.hcpc$desc.axes$quanti
res.hcpc$desc.ind$para


#---------------------------------
# Case of categorical variables
#---------------------------------
# Chargement des données
library(FactoMineR)
data(tea)
# ACM
res.mca <- MCA(tea, 
               ncp = 20,            # Nombre de composants gardés
               quanti.sup = 19,     # Variables quantitatives supplémentaires
               quali.sup = c(20:36), # Variables qualitative supplémentaires
               graph=FALSE)



res.hcpc <- HCPC (res.mca, graph = FALSE, max = 3)

# Dendrogramme
fviz_dend(res.hcpc, show_labels = FALSE)
# Individus
fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

# Description par les variables
res.hcpc$desc.var$test.chi2

# Description par les catégories
res.hcpc$desc.var$category

res.hcpc$desc.axes  # Description by the main axes

res.hcpc$desc.ind$para # Description by individuals

```
