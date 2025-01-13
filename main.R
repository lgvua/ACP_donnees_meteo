clim <- read.csv("https://userpage.fu-berlin.de/soga/300/30100_data_sets/Climfrance.csv", sep = ";")
clim[, "p_mean"] <- as.numeric(gsub(",", "", clim[, "p_mean"]))
clim[, "altitude"] <- as.numeric(gsub(",", "", clim[, "altitude"]))


G1 <- raster::getData(country = "France", level = 1)
library(ggplot2)
ggplot() +
  geom_polygon(data = G1,aes(x = long, y = lat, group = group),colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(data = clim,aes(x = lon, y = lat),alpha = .5,size = 2,color="blue"
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()

# On centre et réduit les variables du tableau clim en enlevant le nom des stations
climcr <- scale(clim[,c(2:12)])


# On creer la matrice de covariance
cov_climcr <- cov(climcr)
cor(climcr)

cov_climcr

# On calcul les valeurs propres et vecteurs propres de la matrice de covariance
eigen_climcr <- eigen(cov_climcr)

#On calcul la proportion de variance expliquée
exp_prop_eig <- eigen_climcr$values/sum(eigen_climcr$values)

# On trace :
# La proportion de variance expliquée
# La proportion cumulée de variance expliquée

plot(exp_prop_eig,ylab="Proportion de variance expliquée",xlab="Composante principale",type="b")

plot(cumsum(exp_prop_eig),ylab="Proportion cumulée de variance expliquée",xlab="Composante principale",type="b")

# Tableau de la proportion cumulé de variance expliquée
exp_prop_eig
cumsum(exp_prop_eig)

# On remarque qu'il nous faut 3 composantes principales (PC1,PC2,PC3) pour expliquer 81,64930% (au moins 80%) de la variance totale

# Faire l'ACP
library("FactoMineR")
res <- PCA(clim[,c(2:12)])

res$var$coord
res$var$cos2
res$var$contrib

library("factoextra")
# Représentation des individus dans le plan principal
fviz_pca_ind(res,col.ind = "black")
# Représentation des variables dans le plan principal
fviz_pca_var(res,col.var = "red")
