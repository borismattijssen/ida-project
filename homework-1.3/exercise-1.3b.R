cars <- read.table("cars.txt", header=TRUE, as.is=TRUE)
cars <- na.omit(cars)
rownames(cars) <- abbreviate(make.unique(cars$name))
cars$origin <- as.factor(cars$origin)
cars.pc <- select(cars, mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin)

library(ggplot2)
library(GGally)
library(FactoMineR)
library(factoextra)

cars.pca=PCA(cars.pc,quali.sup=8,ncp=5,scale.unit=TRUE, graph=FALSE)

# five principal component scores:
# cars.pca$ind$coord

# most variance is explained by the first PC, while the 
cars.pca$eig

fviz_pca_ind(cars.pca, label="none", col.ind = "contrib")
fviz_pca_ind(cars.pca, label="none", habillage="origin",
             addEllipses=TRUE, ellipse.level=0.95)
