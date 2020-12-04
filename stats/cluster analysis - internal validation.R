library(clv)
library(cluster)

data(iris)



# k-means
kmod <- kmeans(iris[,1:4],3) 
plot(iris$Sepal.Length~iris$Sepal.Width, kmod=cluster)
#
hcmod <- hclust(dist(iris))
hcl <- as.integer(cutree(hcmod,3)) 

#intraclust = c("complete","average","centroid")
#interclust = c("single", "complete", "average","centroid")

#######################################
####  Internal Validation
#######################################

# compute intercluster distances and intracluster diameters
icd1 <- cls.scatt.data(iris[,1:4], kmod$cluster, dist="manhattan")
icd2 <- cls.scatt.data(iris[,1:4], hcl, dist="manhattan")

dunn1 <- as.numeric(clv.Dunn(icd1, "complete", "complete"))
davies1 <- as.numeric(clv.Davies.Bouldin(icd1, "complete", "complete"))

dunn2 <- as.numeric(clv.Dunn(icd2, "complete", "complete"))
davies2 <- as.numeric(clv.Davies.Bouldin(icd2, "complete", "complete"))

tab = cbind("Davies Boudin",kmeans=davies1,hclust=davies2)
tab = rbind(tab, cbind("Dunn",kmeans=dunn1,hclust=dunn2))

print(tab)

library(cluster)

silh1 <- silhouette(kmod$cluster,dist(iris))
plot(silh1,col=c(1,2,3))

silh2 <- silhouette(hcl,dist(iris))
plot(silh2,col=c(1,2,3))


########################################
###  External Validation
########################################

### Confusion matrix ( TP,TN,FP,FN )
# Jaccard index [ TP/(TP+FP+FN) ]
# Rand measure [ TP+TN/(TP+FP+FN+TN) ]
# Fowlkes–Mallows index [ TP/√((TP+FP)(TP+FN)) ]

include(clv)

# use only once std.ext function. Check the cluster against real values (iris$species). 
std <- std.ext(kmod$cluster, as.numeric(iris$Species))
# to compute three indicies based on std.ext result
rand1 <- clv.Rand(std)
jaccard1 <- clv.Jaccard(std)
folk.mal1 <- clv.Folkes.Mallows(std)
table(cluster=kmod$cluster, species=iris$Species)

std2 <- std.ext(hcl, as.numeric(iris$Species))
# to compute three indicies based on std.ext result
rand2 <- clv.Rand(std2)
jaccard2 <- clv.Jaccard(std2)
folk.mal2 <- clv.Folkes.Mallows(std2)

res1 <- rbind(Rand=rand1,Jaccard=jaccard1,Fowlkes_Mallows=folk.mal1)
res2 <- rbind(Rand=rand2,Jaccard=jaccard2,Fowlkes_Mallows=folk.mal2)

cbind(kmeans=res1,hclust=res2)


###  Matching based measures
# Purity (% of correctly classified objects)
# F-measure [ 2*precision*recall/(precision + recall) ]


