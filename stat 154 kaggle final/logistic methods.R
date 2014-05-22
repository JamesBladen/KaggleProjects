train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
library(glmnet)




predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)


logistic.cv = cv.glmnet(x = predictors, y = train[,1], family = "multinomial", alpha = 0,  nfolds = 10, type.measure = "class")

logistic.cv$lambda
coefs <- coef(logistic.cv)
plot(logistic.cv)
min(logistic.cv$cvm)


lambda = logistic.cv$lambda.min

my.logistic.fit = glmnet(x = as.matrix(predictors), y = response, family = "multinomial", alpha = 0)


predvals <- predict(my.logistic.fit,predictors,type='class')

sum(diag(table(predvals[,5],response)))/length(response)
