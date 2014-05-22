library(kernlab)
library(caret) #automatic parameter tuning by re-sampling. Very powerful, and handles lots of different model fits. Needs libraries: ggplot2, gtable, plyr, digest, proto, reshape2, stringr, scales, munsell, colorspace, dichromat, labeling, you get the idea, etc. etc. etc.
library(e1071)
library(penalizedLDA)
library(sparseLDA)


train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)


newsvmpredictions <- read.csv("C:/Users/JamesBladen/Desktop/stat 154 kaggle final/New folder/newsvmpredictions.csv")
submission_best_logit <- read.csv("C:/Users/JamesBladen/Desktop/stat 154 kaggle final/New folder/submission_best_logit.csv")
submission_best_rf <- read.csv("C:/Users/JamesBladen/Desktop/stat 154 kaggle final/New folder/submission_best_rf.csv")
svmPolypredictions <- read.csv("C:/Users/JamesBladen/Desktop/stat 154 kaggle final/New folder/svmPolypredictions.csv")

check <- (newsvmpredictions[,2]==submission_best_logit[,2]) + (submission_best_rf[,2]==svmPolypredictions[,2])==2



Rewired <- read.table ("logicalVector")
rowIndex <- as.vector(unlist(Rewired))

newTest <- test[rowIndex==F,]


known <- cbind(newsvmpredictions[rowIndex,2],test[rowIndex,])
names(known) <- names(train)
newTrain <- rbind(train,known)

predictors <- as.matrix(newTrain[,-1])
response <- as.factor(newTrain[,1])
colnames(predictors) <- colnames(test)




polynomialSVM <- train(x=predictors,y=response,method = 'svmPoly')
#The final values used for the model were degree = 3, scale = 0.001 and C = 1. 



testPrediction <- predict(polynomialSVM$finalModel,newdata=test)
testPrediction[rowIndex] <- newsvmpredictions[rowIndex,2]




mean(testPrediction==newsvmpredictions[,2])
mean(testPrediction==submission_best_logit[,2])
mean(testPrediction==svmPolypredictions[,2])
mean(testPrediction==submission_best_rf[,2])





getwd()
submissions = cbind((1:nrow(test)), testPrediction)
write.table(submissions, file = "newMethod", sep = ",", col.names = c("Id", "Predictions"), row.names = F) #I had some trouble getting write.csv to output the submissions in the correct format for kaggle, so I just used write.table and specified a comma as separator.


