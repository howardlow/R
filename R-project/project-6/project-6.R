@@@@@@@@@@@@@@@@@ Q3 @@@@@@@@@@@@@@@@@@@@
#######3a##########
#######3ai##########
library(cluster)
data1 <- read.csv("A3data1.csv")
x <- data1[,-3]

#cluster the data k = 5
km.out=kmeans(x,5,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=5", xlab="", ylab="", pch=20, cex=2)

#######3aii##########
## Hierarchically cluster the data using complete linkage
hc.complete=hclust(dist(x), method="complete")
plot(x,col = cutree(hc.complete, 5), main="Hierarchically Cluster Complete Linkage with K=5", xlab="", ylab="", pch=20, cex=2)
complete.x = cbind(x, cutree(hc.complete, 5))
names(complete.x) <- c("x1", "x2", "Cluster")

## Hierarchically cluster the data using single linkage
hc.single=hclust(dist(x), method="single")
plot(x,col = cutree(hc.single, 5), main="Hierarchically Cluster Single Linkage with K=5", xlab="", ylab="", pch=20, cex=2)
single.x = cbind(x, cutree(hc.single, 5))
names(single.x) <- c("x1", "x2", "Cluster")

#######3aiii##########
par(mfrow =c(1,2))

plot(x, col=data1$Cluster, main="Actual Clustering", xlab="", ylab="", pch=20, cex=2)
plot(x,col = cutree(hc.complete, 5), main="Hierarchically Cluster Complete Linkage with K=5", xlab="", ylab="", pch=20, cex=2)

plot(x, col=data1$Cluster, main="Actual Clustering", xlab="", ylab="", pch=20, cex=2)
plot(x,col = cutree(hc.single, 5), main="Hierarchically Cluster Single Linkage with K=5", xlab="", ylab="", pch=20, cex=2)

table(data1$Cluster, complete.x$Cluster)
table(data1$Cluster, single.x$Cluster)

#######3b##########
#######3bi##########
data2 <- read.csv("A3data2.csv")
x <- data2[,-3]

#cluster the data k = 3
km.out=kmeans(x,3,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

#######3bii##########
## Hierarchically cluster the data using complete linkage
hc.complete=hclust(dist(x), method="complete")
plot(x,col = cutree(hc.complete, 3), main="Hierarchically Cluster Complete Linkage with K=3", xlab="", ylab="", pch=20, cex=2)
complete.x = cbind(x, cutree(hc.complete, 3))
names(complete.x) <- c("x1", "x2", "Cluster")

## Hierarchically cluster the data using single linkage
hc.single=hclust(dist(x), method="single")
plot(x,col = cutree(hc.single, 3), main="Hierarchically Cluster Single Linkage with K=3", xlab="", ylab="", pch=20, cex=2)
single.x = cbind(x, cutree(hc.single, 3))
names(single.x) <- c("x1", "x2", "Cluster")

#######3biii##########
par(mfrow =c(1,2))

plot(x, col=data2$Cluster, main="Actual Clustering", xlab="", ylab="", pch=20, cex=2)
plot(x,col = cutree(hc.complete, 3), main="Hierarchically Cluster Complete Linkage with K=3", xlab="", ylab="", pch=20, cex=2)

plot(x, col=data2$Cluster, main="Actual Clustering", xlab="", ylab="", pch=20, cex=2)
plot(x,col = cutree(hc.single, 3), main="Hierarchically Cluster Single Linkage with K=3", xlab="", ylab="", pch=20, cex=2)

table(data2$Cluster, complete.x$Cluster)
table(data2$Cluster, single.x$Cluster)

