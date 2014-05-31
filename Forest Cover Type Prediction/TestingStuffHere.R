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




successRate <- rep(0,100)
for(k in 1:100){
  for(i in 1:5){
    predvals <- knn(train=trainPredictors[-kfolds[[i]],],test=trainPredictors[kfolds[[i]],],cl=trainResponse[-kfolds[[i]]],k=k)
    successRate[k] <- sum(predvals==trainResponse[kfolds[[i]]]) + successRate[k]
  }
}
bestK <- sort(successRate,index.return=TRUE,decreasing=TRUE)$ix[1]
testPred <- knn(train=trainPredictors,test=testPredictors,cl=trainResponse,k=1)
mean(testPred==testResponse)




predictions <- knn(train=train[,2:55],test=test[,-1],cl=train[,56])
submissions = cbind(test[,1], predictions)
write.table(submissions, file = "submission.csv", sep = ",", col.names = c("Id", "Cover_Type"), row.names = F)





################################################################################



library(glmnet)

logistic.cv = cv.glmnet(x = as.matrix(trainPredictors), y = trainResponse, family = "multinomial", alpha = 0,  nfolds = 10, type.measure = "class")
lambda = logistic.cv$lambda.min



my.logistic.fit = glmnet(x = as.matrix(trainPredictors), y = trainResponse, family = "multinomial", alpha = 0)
logisitcpredvals <- predict(my.logistic.fit,as.matrix(testPredictors),type='class')

thebest <- vector()
for(l in 1:ncol(logisitcpredvals)){
  thebest[l] <- mean(logisitcpredvals[,l]==testResponse)
}


mean(logisitcpredvals==testResponse)





#########################################################################################



#Tree Methods






library(rpart)


mydata <- cbind(as.factor(trainResponse),trainPredictors)
colnames(mydata)[1] <- "response"

tree <- rpart(response~.,data=mydata, control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))
plotcp(tree)
tree.pruned = prune(tree, cp = tree$cptable[66,1])


treePred <- predict(tree.pruned,testPredictors,type="class")

mean(testResponse==treePred)







############################################################


library(randomForest)
?randomForest
rf <- randomForest(x=trainPredictors,y=as.factor(trainResponse),ntree=1000)
randomForestPredictions <- predict(rf,as.matrix(testPredictors))



mean(randomForestPredictions==testResponse)


rfPredictions <- predict(rf,as.matrix(test))





submissions = cbind(test[,1], rfPredictions)
write.table(submissions, file = "randomForest.csv", sep = ",", col.names = c("Id", "Cover_Type"), row.names = F)










#############################################################################





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
