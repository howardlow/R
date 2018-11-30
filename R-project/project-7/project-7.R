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


