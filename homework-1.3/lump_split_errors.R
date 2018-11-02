class.dev <- c(1,1,2,1,3)
cluster.dev <-c(1,2,1,1,3)

# the two functions could be optimized performing operations only on half of the matrices,
# since they are simmetric

count.lump.errors <- function (class, cluster) {
  matrix.class.row <- matrix(class,length(class),length(class), byrow = T)
  matrix.class.col <- matrix(class,length(class),length(class), byrow = F)
  matrix.class.same <- matrix.class.col==matrix.class.row
  
  matrix.cluster.row <- matrix(cluster,length(cluster),length(cluster), byrow = T)
  matrix.cluster.col <- matrix(cluster,length(cluster),length(cluster), byrow = F)
  matrix.cluster.same <- matrix.cluster.col==matrix.cluster.row
  
  matrix.lump <- matrix.cluster.same==T & matrix.class.same==F
  n.lump <- sum(matrix.lump==T) / 2
  return(n.lump)
}

count.split.errors <- function (class, cluster) {
  
  matrix.class.row <- matrix(class,length(class),length(class), byrow = T)
  matrix.class.col <- matrix(class,length(class),length(class), byrow = F)
  matrix.class.same <- matrix.class.col==matrix.class.row
  
  matrix.cluster.row <- matrix(cluster,length(cluster),length(cluster), byrow = T)
  matrix.cluster.col <- matrix(cluster,length(cluster),length(cluster), byrow = F)
  matrix.cluster.same <- matrix.cluster.col==matrix.cluster.row

  matrix.split <- matrix.class.same==T & matrix.cluster.same==F
  n.split <- sum(matrix.split==T) / 2
  return(n.split)
}

class.test <- c(1,2,2)
cluster1 <- c(1,2,2)
cluster2 <- c(2,1,1)
cluster3 <- c(4,5,6)
cluster4 <-c(1,1,1)

count.lump.errors(class.test, cluster1)
count.lump.errors(class.test, cluster2)
count.lump.errors(class.test, cluster3)
count.lump.errors(class.test, cluster4)
