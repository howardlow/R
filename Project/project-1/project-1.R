setwd("E:/School/UC/STAT448S1 - Big Data/Assignment/Assignment2")

#a)
load("Residen.RData")
fit<-lm(V104~. -V105 ,data=Residen)
summary(fit)


################################################################################

#b)

library(MASS)

#backwards selection
fit<-lm(V104~. -V105 ,data=Residen)
step1 <- stepAIC(fit, direction="backward")

#stepwise selection
fit<-lm(V104~. -V105 ,data=Residen)
step2 <- stepAIC(fit, direction="both")

#display output
summary(step1)
summary(step2)

#Computational time
system.time(stepAIC(fit, direction="backward"))
system.time(stepAIC(fit, direction="both"))

#Holdout MSE
#data prep
set.seed(3)
row.number <- sample(1:nrow(Residen), 0.8*nrow(Residen)) 
train = Residen[row.number,]
test = Residen[-row.number,] 
fit<-lm(V104~. -V105 ,data=train)

#backwards
step1 <- stepAIC(fit, direction="backward")
pred1 <- predict(step1, newdata = test)
mse1 <- sum((pred1 - test$V104)^2)/length(test$V104)
mse1

#stepwise
step2 <- stepAIC(fit, direction="both")
pred2 <- predict(step2, newdata = test)
mse2 <- sum((pred2 - test$V104)^2)/length(test$V104)
mse2


#Cross validation MSE
fit<-lm(V104~. -V105 ,data=Residen)
step1 <- stepAIC(fit, direction="backward")
step2 <- stepAIC(fit, direction="both")

library(DAAG) 
cv1 <- cv.lm(data=Residen, step1, m=10)
attr(cv1, "ms")

cv2 <- cv.lm(data=Residen, step2, m=10)
attr(cv2, "ms")

