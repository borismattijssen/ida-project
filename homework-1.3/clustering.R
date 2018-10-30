# loading libraries
library("tidyverse")
library("factoextra")
library("cluster")
library("GGally")

###### Kmeans
###### pam 
###### to select the number of clusters: package NbClust
###### Model based: mclust


#loading and scaling data


car5_scale=scale(car5, center=FALSE) #sottrai la media e dividi per la varianza
dist.car5=get_dist(car5_scale)
# visualizing a matrix of distances
fviz_dist(dist.car5)



## Partitioning clustering

#k=3:
car5.k3 <- kmeans(car5_scale, centers=3, iter.max=100, nstart=25)
pairs(car5, col=car5.k3$cluster)

# We can also get the average silhouette width with kmeans:
summary(silhouette(car5.k3$cluster,dist.car5))$avg.width


# visualizaing the clusters via PCA
fviz_cluster(list(data=car5,cluster=car5.k4$cluster))

# k-medoids clustering
# K-medoids directly on the (standardized) data matrix:
car5.kmed.3 <- pam(car5_scale, k=3, diss=F)
pairs(car5, col=car5.kmed.3$cluster)

# silhouette: This shows which observations are "best clustered"
plot(car5.kmed.3, which.plots=2)
# A LARGE average silhouette width indicates that the observations are properly clustered.
car5.kmed.3$silinfo$avg.width  #printing the average silhouette width
# k=3 it is doing poorly. Try k=4.
car5.kmed.4 <- pam(car5_scale, k=4, diss=F)
car5.kmed.4$silinfo$avg.width

# determining optimal number of clusters: three methods studied: silhouette, wss, gap
fviz_nbclust(car5_scale, kmeans, method = "silhouette")

fviz_nbclust(car5_scale, kmeans, method = "wss")

fviz_nbclust(car5_scale, kmeans, method = "gap")

# Gap statistic in more detail
gap_stat <- clusGap(car5_scale, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat)

# NbClust: A Package providing 30 indices for determining the best number of clusters
library("NbClust")

res.nb.kmeans.ch <- NbClust(car5_scale, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "kmeans", index ="ch") 


res.nb.kmeans.si <- NbClust(car5_scale, distance = "euclidean",
                            min.nc = 2, max.nc = 10, 
                            method = "kmeans", index ="silhouette") 

res.nb.kmeans.gap <- NbClust(car5_scale, distance = "euclidean",
                            min.nc = 2, max.nc = 10, 
                            method = "kmeans", index ="gap")

# There are 30 indexes defined to selct the "best" number of clusters:
res.nb <- NbClust(car5_scale, distance = "euclidean", min.nc = 2, 
                  max.nc = 10, 
                  method = "kmeans", index ="all")

fviz_nbclust(res.nb)

# Library factoextra contains a function hkmeans() to combine hierarchical and kmeans

# Model-based clustering: library mclust
# Consider the built-in diabetes data set in mclust:
# We will perform a model-based clustering of the 50 states based on these 4 variables:
library(mclust)
data("diabetes")
head(diabetes)

diabetesModel <- Mclust(diabetes[,-1])
plot(diabetesModel)

coordProj(diabetes[,-1], dimens = c(2,3), what = "classification",
          classification = diabetesModel$classification,
          parameters = diabetesModel$parameters)

coordProj(diabetes[,-1], dimens = c(2,3), what = "errors",
          classification = diabetesModel$classification,
          parameters = diabetesModel$parameters,
          truth = diabetes[,1])

library(RColorBrewer)
library(factoextra)
fviz_mclust(diabetesModel, "BIC", palette = "Dark2")
fviz_mclust(diabetesModel, "classification", geom = "point", 
            pointsize = 1.5, palette = "Dark2")
fviz_mclust(diabetesModel, "uncertainty", palette = "Dark2")
