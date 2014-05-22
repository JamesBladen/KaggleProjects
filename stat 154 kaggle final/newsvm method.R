library(kernlab)
library(caret) 


train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
newsvmpredictions <- read.csv("~/newsvmpredictions.csv")
#training <- createDataPartition(train[,1],p=.75,list=F)


predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)

data <- data.frame(cbind(response,predictors))
svm.model.1 = ksvm(as.factor(response)~predictors,data=data, cross = 20) 
mean(predict(svm.model.1,newdata=predictors)==response)


testpredictions <- predict(svm.model.1,newdata=as.matrix(test))
mean(testpredictions==newsvmpredictions[,2])




tuneControl = trainControl(method = "repeatedcv", number = 10, repeats = 10) 
svm.model.2 <- train(as.factor(response)~., data = data, method = "svmRadial", trControl = tuneControl) 

svm.model.2
svm.model.2$results

mean(response==predict(svm.model.2$finalModel, newdata = as.matrix(predictors))) #same results
new2testpredictions <- predict(svm.model.2$finalModel,newdata=as.matrix(test))
mean(new2testpredictions==newsvmpredictions[,2])
mean(new2testpredictions==testpredictions)



getwd()
submissions = cbind((1:nrow(test)), newtestpredictions)
write.table(submissions, file = "justatest.csv", sep = ",", col.names = c("Id", "Predictions"), row.names = F) #I had some trouble getting write.csv to output the submissions in the correct format for kaggle, so I just used write.table and specified a comma as separator.


