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






library(kernlab)

topVariables <- order(rf$importance,decreasing=TRUE)[1:10]


counter <- 1

ksvmSuccess <- matrix(0,nrow=100,ncol=4)
for(degree in 2:7){
  for(scale in c(.001,.01,.1)){
    for (C in c(.1,.5,1)){
      polyModel <- ksvm(x=as.matrix(trainPredictors[,topVariables]),y=as.factor(trainResponse),kernel='polydot',kpar=list(degree=degree,scale=scale,offset=1), C=C)
      polyPred <- predict(polyModel,newdata=testPredictors[,topVariables])
      correct <- mean(polyPred==testResponse)
      ksvmSuccess[counter,1] <- degree
      ksvmSuccess[counter,2] <- scale
      ksvmSuccess[counter,3] <- C
      ksvmSuccess[counter,4] <- correct
      
      counter <- counter+1
    }
  }
}
colnames(ksvmSuccess) <- c("degree","Scale","Cost","Success Rate")








order(ksvmSuccess[,4],decreasing=TRUE)



polyModel <- ksvm(x=as.matrix(trainPredictors[,topVariables]),y=as.factor(trainResponse),kernel='polydot',kpar=list(degree=7,scale=.1,offset=1), C=.5)
polyPred <- predict(polyModel,newdata=test[,topVariables])





submissions = cbind(test[,1], polyPred)
write.table(submissions, file = "polySVMPredictions.csv", sep = ",", col.names = c("Id", "Cover_Type"), row.names = F)
