setwd("E:/School/UC/STAT448S1 - Big Data/Assignment/Assignment3")
library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models

######## 1)
data_comp <- read.csv("Predict_SCHCONN1.csv") 
library(corrplot) 
corrplot(cor(data_comp), method = "number")

cor(y=data_comp$SCHCONN1, data_comp)

######## 2)
attach(data_comp) 
set.seed(100) 
data_comp_scaled <- data.frame(scale(data_comp)) 
X <- model.matrix(SCHCONN1~., data_comp_scaled) 
y <- data_comp$SCHCONN1 
lambda <- 10^seq(4, -2, length = 100)
train = sample(1:nrow(X), nrow(X)/2) 
test = (-train) 
ytest = y[test]

#OLS 
SchConnlm <- lm(SCHCONN1~., data = data.frame(X), subset = train) 
ols.coef <- coef(SchConnlm)
ols.coef


######## 3)
#Ridge parameter lambda tuning, and regression 
ridge.mod <- glmnet(X[train,], y[train], alpha = 0, lambda = lambda) 
#find the best lambda from our list via cross-validation 
cv.ridge.out <- cv.glmnet(X[train,], y[train], alpha = 0)
plot(cv.ridge.out$glmnet.fit, "lambda", label=TRUE)
bestlam <- cv.ridge.out$lambda.min

#make predictions 
ridge.pred <- predict(ridge.mod, s = bestlam, newx = X[test,]) 
ols.pred <- predict(SchConnlm, newdata = data.frame(X[test,]))


#check MSE and variance 
mean((ols.pred-ytest)^2) 
mean((ridge.pred-ytest)^2) 
var(ols.pred) 
var(ridge.pred)
var(ytest)
ridge.coef <- predict(cv.ridge.out, type = "coefficients", s = bestlam)

######## 4)
lasso.mod <- glmnet(X[train,], y[train], alpha = 1, lambda = lambda)
cv.lasso.out <- cv.glmnet(X[train,], y[train], alpha = 1)
plot(cv.lasso.out$glmnet.fit, "lambda", label=TRUE) 
lasso.coef <- predict(cv.lasso.out, type = "coefficients", s = cv.lasso.out$lambda.min)

elastic.mod <- glmnet(X[train,], y[train], alpha = 0.5, lambda = lambda) 
cv.elastic.out <- cv.glmnet(X[train,], y[train], alpha = 0.5) 
plot(cv.elastic.out$glmnet.fit, "lambda", label=TRUE) 
elastic.coef <- predict(cv.elastic.out, type = "coefficients", s = cv.elastic.out$lambda.min)


######## 5)
lasso.pred <- predict(lasso.mod, s = cv.lasso.out$lambda.min, newx = X[test,]) 
elastic.pred <- predict(elastic.mod, s = cv.elastic.out$lambda.min, newx = X[test,]) 
cv.ridge.out$lambda.min
cv.lasso.out$lambda.min
cv.elastic.out$lambda.min
mean((ols.pred-ytest)^2) 
mean((ridge.pred-ytest)^2) 
mean((lasso.pred-ytest)^2) 
mean((elastic.pred-ytest)^2) 
var(ols.pred) 
var(ridge.pred)
var(lasso.pred)
var(elastic.pred)


####### 6)
library(corrplot) 
library(glmnet)
data <- read.csv("communities_crime.data", header=FALSE)
cdata = data[sapply(data, function(x) !is.factor(x))]
corrplot(cor(cdata), method = "number")
cor(y=data$V128, cdata)

####### 7)
attach(data) 
set.seed(100) 
data_comp_scaled <- data.frame(scale(cdata)) 
X <- model.matrix(V128~., data_comp_scaled) 
y <- data$V128 
lambda <- 10^seq(4, -2, length = 100)
train = sample(1:nrow(X), nrow(X)/2) 
test = (-train) 
ytest = y[test]

#OLS
ols.mod <- lm(V128~., data = data.frame(X), subset = train) 
ols.pred <- predict(ols.mod, newdata = data.frame(X[test,]))

#Ridge
ridge.mod <- glmnet(X[train,], y[train], alpha = 0, lambda = lambda) 
cv.ridge.out <- cv.glmnet(X[train,], y[train], alpha = 0)
ridge.pred <- predict(ridge.mod, s = cv.ridge.out$lambda.min, newx = X[test,]) 
cv.ridge.out$lambda.min

#LASSO
lasso.mod <- glmnet(X[train,], y[train], alpha = 1, lambda = lambda)
cv.lasso.out <- cv.glmnet(X[train,], y[train], alpha = 1)
lasso.pred <- predict(lasso.mod, s = cv.lasso.out$lambda.min, newx = X[test,]) 
cv.lasso.out$lambda.min

#Elastic
elastic.mod <- glmnet(X[train,], y[train], alpha = 0.5, lambda = lambda) 
cv.elastic.out <- cv.glmnet(X[train,], y[train], alpha = 0.5) 
elastic.pred <- predict(elastic.mod, s = cv.elastic.out$lambda.min, newx = X[test,]) 
cv.elastic.out$lambda.min

#test
mean((ols.pred-ytest)^2) 
mean((ridge.pred-ytest)^2) 
mean((lasso.pred-ytest)^2) 
mean((elastic.pred-ytest)^2) 
var(ols.pred) 
var(ridge.pred)
var(lasso.pred)
var(elastic.pred)

ols.coef <- coef(ols.mod)
ridge.coef <- predict(cv.ridge.out, type = "coefficients", s = cv.ridge.out$lambda.min)
lasso.coef <- predict(cv.lasso.out, type = "coefficients", s = cv.lasso.out$lambda.min)
elastic.coef <- predict(cv.elastic.out, type = "coefficients", s = cv.elastic.out$lambda.min)



