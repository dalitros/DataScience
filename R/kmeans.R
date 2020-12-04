View(iris)

kmod <- kmeans(as.matrix(iris[,1:4]),center=3)
table(kmod$cluster)
table(iris$cluster)
kmod$centers
kmod$tot.withiness
plot(iris$Sepal.Length, iris$Sepal.Width, color=iris$species)
plot(iris$Petal.Length, iris$Petal.Width, color=iris$species)
plot(iris$Petal.Length, iris$Setal.Width, color=iris$species)

kmod2 <- kmeans(as.matrix(iris[,1:4]),center=2)
table(kmod2$cluster)

plotElbow <- function(data,kmax=8)
#compute and plot wss for k=2 to kmax
  set.seed(123)
  wss <- sapply(1:kmax,
                function(k){kmeans(data,k,nstart = 50, iter.max= 15)$tot.withinss})
  wss
  plot(1:kmax,wss,
       type="b"pch=19, frame=FALSE, 
       xlab="Number of clusters K"
       ylab="Total within-clusters sum of squares")
  }
plotElbow(iris[,1:4], kmax=8)

library(NbClust)


res <- NbClust(iris[,1:4], distance = "euclidean",min.nc=2, max.nc=8, method = "complete")

#dendogram 
hcmod <- hclust(d=dist(iris[,1:4]))
hcmod
 plot(hcmod)
 
 hcmod <- hclust(d=dist(iris[,1:4],method ="camberra"))
 cluster_hc <-cutree()
 plot(iris$Petal.Length, iris$Petal.Width, col=cluster_hc)
 
#DBSCAN
 library(dbscan)
 
 dbsmod <-dbscan(iris[,1:4],eps=0.8,minPts =5)
 dbsmod
 
 table(dbsmod$cluster)
plot(iris$Petal.Length, iris$petal_width, col=dbsmod$cluster+1) 
