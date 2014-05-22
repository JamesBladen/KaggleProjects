setwd("C:/Users/User Files/Desktop")
library(class)
library(caret)


train <- read.csv("C:/Users/User Files/Desktop/train.csv")
test <- read.csv("C:/Users/User Files/Desktop/test.csv")

sampleSubmission <- read.csv("C:/Users/User Files/Desktop/sampleSubmission.csv")


smallTestSet <- vector()
for(j in 1:7){
  smallTestSet <- c(smallTestSet,sample(which(response==j),size=216))
}
newTrain <- train[-smallTestSet,]
newTest <- train[smallTestSet,]


trainPredictors <- newTrain[,2:55]
trainResponse <- newTrain[,56]

testPredictors <- newTest[,2:55]
testResponse <- newTest[,56]




kfolds <- createFolds(trainResponse,k=5)




successRate <- rep(0,100)
for(k in 1:100){
  for(i in 1:5){
    predvals <- knn(train=trainPredictors[-kfolds[[i]],],test=trainPredictors[kfolds[[i]],],cl=trainResponse[-kfolds[[i]]],k=k)
    successRate[k] <- sum(predvals==trainResponse[kfolds[[i]]]) + successRate[k]
  }
}
bestK <- sort(successRate,index.return=TRUE,decreasing=TRUE)$ix[1]
testPred <- knn(train=trainPredictors,test=testPredictors,cl=trainResponse)
mean(testPred==testResponse)




predictions <- knn(train=train[,2:55],test=test[,-1],cl=train[,56])
submissions = cbind(test[,1], predictions)
write.table(submissions, file = "submission.csv", sep = ",", col.names = c("Id", "Cover_Type"), row.names = F)



























