setwd("C:/Users/User Files/Desktop")
library(class)
library(caret)


train <- read.csv("C:/Users/User Files/Desktop/train.csv")
test <- read.csv("C:/Users/User Files/Desktop/test.csv")

sampleSubmission <- read.csv("C:/Users/User Files/Desktop/sampleSubmission.csv")


smallTestSet <- vector()
for(j in 1:7){
  smallTestSet <- c(smallTestSet,sample(which(train[,56]==j),size=216))
}
newTrain <- train[-smallTestSet,]
newTest <- train[smallTestSet,]


trainPredictors <- newTrain[,2:55]
trainResponse <- newTrain[,56]

testPredictors <- newTest[,2:55]
testResponse <- newTest[,56]




kfolds <- createFolds(trainResponse,k=5)




############################################################


library(randomForest)
?randomForest
rf <- randomForest(x=trainPredictors,y=as.factor(trainResponse),ntree=1000)
randomForestPredictions <- predict(rf,as.matrix(testPredictors))



mean(randomForestPredictions==testResponse)


rfPredictions <- predict(rf,as.matrix(test))





submissions = cbind(test[,1], rfPredictions)
write.table(submissions, file = "randomForest.csv", sep = ",", col.names = c("Id", "Cover_Type"), row.names = F)


