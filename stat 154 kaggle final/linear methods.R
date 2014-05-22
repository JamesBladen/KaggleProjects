train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
library(randomForest)
library(class)
library(rpart)
library(glmnet)




predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)



mylm <- lm(train[,1]~predictors)
summary(mylm)














