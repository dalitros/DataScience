View(iris)


kmod <- kmeans(as.matrix(iris[,1:4]),centers=3)

table(kmod$cluster)
kmod$centers
kmod$tot.withinss

plot(iris$sepal_length, iris$sepal_width, col=iris$species)
plot(iris$petal_length, iris$petal_width, col=iris$species)
plot(iris$petal_length, iris$petal_width, col=kmod$cluster)


kmod2 <- kmeans(as.matrix(iris[,1:4]),centers=2)

table(kmod2$cluster)
kmod$centers
kmod$tot.withinss

plot(iris$petal_length, iris$petal_width, col=kmod2$cluster)


plotElbow <- function(data, kmax=8) {
  # Compute and plot wss for k = 2 to kmax
  set.seed(123)
  wss <- sapply(1:kmax, 
                function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
  wss
  plot(1:kmax, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")    
}

plotElbow(iris[,1:4], kmax = 8)

library(NbClust)


res <- NbClust(iris[,1:4], distance = "euclidean", min.nc=2, max.nc=8, method = "complete")


hcmod <- hclust(d = dist(iris[,1:4],method ="canberra" ))
cluster_hc <- cutree(hcmod, 3)

plot(hcmod)

table(cluster_hc)
plot(iris$petal_length, iris$petal_width, col=cluster_hc)

library(dbscan)

dbsmod <- dbscan(iris[,1:4],eps=0.8,minPts =5)
dbsmod

table(dbsmod$cluster)

plot(iris$petal_length, iris$petal_width, col=dbsmod$cluster+1)

####################################################
pca <- prcomp(iris[,1:4])

plot(pca)
pca$sdev
pca$sdev/sum(pca$sdev)

plot(pca$x)
plot(pca$x,col=iris$Species)
plot(pca$x[,3]~ pca$x[,1],col=iris$Species)

kmod <- kmeans(pca$x[,1:2],3)
plot(pca$x,col=kmod$cluster)
table(clust=kmod$cluster, species=iris$Species)
