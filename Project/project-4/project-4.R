#Q1a)
p = (exp(-16+(1.4*5)+(0.3*36))) / (1+exp(-16+(1.4*5)+(0.3*36)))

#2a)
bankTrain <- read.csv("BankTrain.csv") 
fit <- glm(y~x1+x3,data=bankTrain,family=binomial(link='logit'))
summary(fit)

#2b)
library(boot)
bankTest <- read.csv("BankTest.csv") 
bankFull <- rbind(bankTrain, bankTest)

boot.fn=function(data,index){
  return(coef(glm(y~x1+x3, data=data,subset=index)))
}
boot.fn(bankFull,1:1372)
set.seed(1)
boot.fn(bankFull,sample(1372,1372,replace=T))

boot(bankFull,boot.fn,1000)
summary(glm(y~x1+x3, data=bankFull))$coef

#2ci)
library(ggplot2)
threshold = 0.5

slope <- coef(fit)[2]/(-coef(fit)[3])
#intercept1 <- coef(fit)[1]/(-coef(fit)[3]) 
intercept <- (log(threshold/(1-threshold)) - coef(fit)[1]/(-coef(fit)[3])) 

base<-ggplot()+geom_point(aes(x=bankTrain$x1,y=bankTrain$x3,color=factor(bankTrain$y),shape=factor(bankTrain$y)),size=3)
base+geom_abline(intercept = intercept , slope = slope, color = "red", size = 2) 

#2cii)
library(caret)
threshold = 0.5
probs.test = predict(fit,bankTest,type="response")

pred.test = rep(0,412)
pred.test[probs.test>threshold]=1

confusionMatrix(as.factor(pred.test),as.factor(bankTest$y))


#2ciii)

#calculate optimal threshold
cutoffs <- seq(0.1,0.9,0.01)
accuracy <- NULL
for (i in seq(along = cutoffs)){
    prediction <- ifelse(fit$fitted.values >= cutoffs[i], 1, 0) #Predicting for cut-off
    accuracy <- c(accuracy,length(which(bankTrain$y ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")

threholds <- as.data.frame(cbind(cutoffs, accuracy))
threholds[which.max(threholds$accuracy),]

#confusion matrix of using 0.41 threshold for testing set
threshold = 0.41
probs.test = predict(fit,bankTest,type="response")

pred.test = rep(0,412)
pred.test[probs.test>threshold]=1

confusionMatrix(as.factor(pred.test),as.factor(bankTest$y))


#confusion matrix of using 0.41 threshold for training set
threshold = 0.41
probs.train = predict(fit,bankTrain,type="response")

pred.train = rep(0,960)
pred.train[probs.train>threshold]=1

confusionMatrix(as.factor(pred.train),as.factor(bankTrain$y))

mean(pred.test != bankTest$y)
mean(pred.train != bankTrain$y)


#3a)
library(MASS)
lda.fit<-lda(y~x1+x3,bankTrain)
lda.fit

lda.trainpred<-predict(lda.fit,bankTrain)
lda.testpred<-predict(lda.fit,bankTest)

mean(lda.trainpred$class!=bankTrain$y)
mean(lda.testpred$class!=bankTest$y)


#3b)
qda.fit<-qda(y~x1+x3,bankTrain)
qda.fit

qda.trainpred<-predict(qda.fit,bankTrain)
qda.testpred<-predict(qda.fit,bankTest)

mean(qda.trainpred$class!=bankTrain$y)
mean(qda.testpred$class!=bankTest$y)


#4
x <- 1- log(1.5) *2
p0 <- 0.4 * pnorm(x, mean = 0, sd = 2, lower.tail = FALSE)
p1 <- 0.6 * pnorm(x, mean = 2, sd = 2, lower.tail = TRUE)

p <- p0 + p1
p



#plotting
xs <- seq(-8,10, 0.01)
f0 <- (1/sqrt(8*pi))*exp(-xs^2/8)
f1 <- (1/sqrt(8*pi))*exp(-(xs-2)^2/8)
pi0 <- 0.4
pi1 <- 0.6
y0 <- f0 * pi0
y1 <- f1 * pi1
plot(xs, y1, type = "l",col = "dark green", lwd = 2, xlab = "x", 
     ylab = "pi * f(x)",
     main = "Plot showing Bayes error rate in Binary classification")
lines(xs, y0, type = "l",col = "purple", lwd = 2)
polygon(xs, pmin(y0, y1), col="grey")
x <- 1 - 2 * log(1.5)
abline(v = x, lty = 2, col = "red", lwd = 2)

legend("topleft", legend = c("Class 1", "Class 0", "Decision Boundary"),
       col = c("dark green", "purple", "red"), lty = c(1,1,2), lwd = c(2,2,2))

#0.2945


