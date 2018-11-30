## STAT318/462 kNN regression function
library(ggplot2)

wagesTrain <- read.csv("WageTrain.csv") 
wagesTest <- read.csv("WageTest.csv") 

Xtrain <- wagesTrain$age
Ytrain <- wagesTrain$wage

Xtest <- wagesTest$age
Ytest <- wagesTest$wage

#5a
K = c(1,5,10,20,50,75,100,150,200,300)
idx = 1
MSE = numeric(length(K))
for (i in K){
  pred_y = kNN(i,Xtrain,Ytrain,Xtest)
  MSE[idx] <- mean((pred_y - Ytest)^2)
  idx = idx + 1
}

#plot(K, MSE)
df = data.frame(MSE,1/K)
ggplot(df,aes(1/K,MSE))+ geom_line(colour = "black") + xlab("1/k") +ylab("MSE")+geom_point(colour = "blue")


#5b
fit <- loess(wage ~ age, wagesTrain, control = loess.control(surface = "direct"))
summary(fit)

pred_y = predict(fit, wagesTest)
MSE_loess = mean((c(pred_y) - Ytest)^2)
MSE_loess


#5c
full <- rbind(wagesTrain,wagesTest)
age <- full$age
wage <- full$wage

predY.knn = kNN(150,age,wage,age)
loess.full.fit <- loess(wage ~ age, full, control = loess.control(surface = "direct"))
predY.loess = predict(loess.full.fit, full)

knn.df = data.frame(age, predY.knn)
loess.df = data.frame(age, predY.loess)

ggplot() + geom_point() + 
  geom_line(data=knn.df, aes(x=age, y=predY.knn), color='blue') + 
  geom_line(data=loess.df, aes(x=age, y=predY.loess), color='black', linetype="dotted") +
  xlab("age") + ylab("wage") 
  


kNN <- function(k,x.train,y.train,x.pred) {
# 
## This is kNN regression function for problems with
## 1 predictor
#
## INPUTS

# k       = number of observations in nieghbourhood 
# x.train = vector of training predictor values
# y.train = vector of training response values
# x.pred  = vector of predictor inputs with unknown
#           response values 
#
## OUTPUT
#
# y.pred  = predicted response values for x.pred

## Initialize:
n.pred <- length(x.pred);		y.pred <- numeric(n.pred)

## Main Loop
for (i in 1:n.pred){
  d <- abs(x.train - x.pred[i])
  dstar = d[order(d)[k]]
  y.pred[i] <- mean(y.train[d <= dstar])		
}
## Return the vector of predictions
invisible(y.pred)
}