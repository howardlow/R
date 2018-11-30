#@@@@@@@@@@@@@@@@@ Q2 @@@@@@@@@@@@@@@@@@@@
library(tree)

#######2a##########
carTrain <- read.csv("carTrain.csv")
carTest <- read.csv("carTest.csv")

tree.car.fit =tree(Sales~.,carTrain)
summary(tree.car.fit)
plot(tree.car.fit)
text(tree.car.fit,pretty=0)

#MSE for training set
tree.pred.train=predict(tree.car.fit,carTrain)
mean((tree.pred.train - carTrain$Sales)^2)

#MSE for testing set
tree.pred.test=predict(tree.car.fit,carTest)
mean((tree.pred.test -carTest$Sales)^2)

#######2b##########
#PRUNING
set.seed (3)
cv.car =cv.tree(tree.car.fit)
plot(cv.car$size,cv.car$dev,type="b")
#best is 9
prune.car =prune.tree(tree.car.fit,best =9)
plot(prune.car)
text(prune.car,pretty =0)

#MSE for testing set
tree.pred.train=predict(prune.car, carTrain)
mean((tree.pred.train -carTrain$Sales)^2)

#MSE for testing set
tree.pred.test=predict(prune.car, carTest)
mean((tree.pred.test -carTest$Sales)^2)

#######2c##########
library (randomForest)
set.seed (1)

#The argument mtry=9 indicates that all 9 predictors should be considered for each split of the tree-in other words, that bagging should be done.
#BAGGING
bag.car.fit =randomForest(Sales~.,data=carTrain ,mtry=9, importance =TRUE)
bag.car.fit

#MSE for training set
bag.pred.train = predict (bag.car.fit ,newdata =carTrain)
mean(( bag.pred.train - carTrain$Sales)^2)

#MSE for testing set
bag.pred.test = predict (bag.car.fit ,newdata =carTest)
mean(( bag.pred.test - carTest$Sales)^2)

#RANDOM FOREST
set.seed (1)
rf.car.fit =randomForest(Sales~.,data=carTrain , mtry=4, importance =TRUE)

#MSE for training set
rf.pred.train = predict (rf.car.fit ,newdata =carTrain)
mean(( rf.pred.train - carTrain$Sales)^2)

#MSE for testing set
rf.pred.test = predict (rf.car.fit ,newdata =carTest)
mean(( rf.pred.test - carTest$Sales)^2)

importance(rf.car.fit)
varImpPlot (rf.car.fit)

#######2d##########
#BOOSTING
library(gbm)

set.seed (1)
boost.car.fit =gbm(Sales~.,data=carTrain,distribution="gaussian",n.trees =1500
                   ,interaction.depth=1, shrinkage=0.01,verbose=F)
summary(boost.car.fit)

#par(mfrow =c(1,2))
plot(boost.car.fit ,i="Price")
plot(boost.car.fit ,i="CompPrice")

#MSE for training set
boost.pred.train = predict (boost.car.fit ,newdata =carTrain, n.trees =1500)
mean(( boost.pred.train - carTrain$Sales)^2)

#MSE for testing set
boost.pred.test = predict (boost.car.fit ,newdata =carTest, n.trees =1500)
mean(( boost.pred.test - carTest$Sales)^2)

#@@@@@@@@@@@@@@@@@ Q3 @@@@@@@@@@@@@@@@@@@@
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

#@@@@@@@@@@@@@@@@@ Q4 @@@@@@@@@@@@@@@@@@@@
library(ggplot2)
library(e1071)

bankTrain <- read.csv("BankTrain.csv") 
bankTest <- read.csv("BankTest.csv") 
bankTrain <- bankTrain[c('x1', 'x3', 'y')]
bankTest <- bankTest[c('x1', 'x3', 'y')]
bankTrain$y <-as.factor (bankTrain$y)
bankTest$y <-as.factor (bankTest$y)

#4a)
base<-ggplot()+geom_point(aes(x=bankTrain$x3,y=bankTrain$x1,color=factor(bankTrain$y),shape=factor(bankTrain$y)),size=3)
base

#4b)
svmfit =svm(y~., data=bankTrain , kernel ="linear", cost =10,scale =FALSE )
plot(svmfit, bankTrain)

summary(svmfit)

set.seed(1)
tune.out=tune(svm,y~.,data=bankTrain ,kernel ="linear",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)
# cost
#0.1

plot(bestmod, bankTrain)

ypred= predict(bestmod ,bankTest)
table(predict =ypred , truth= bankTest$y)

#4c)
svmfit =svm(y~., data=bankTrain, kernel ="radial", gamma =1, cost =1)
plot(svmfit , bankTrain)
summary(svmfit)

set.seed (1)
tune.out=tune(svm, y~., data=bankTrain, kernel ="radial", ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000), gamma=c(0.5,1,2,3,4) ))
summary (tune.out)
#
#cost gamma
#10     4

bestmod = tune.out$best.model
summary(bestmod)

plot(bestmod, bankTrain)

ypred= predict(bestmod ,bankTest)
table(predict =ypred , truth= bankTest$y)


