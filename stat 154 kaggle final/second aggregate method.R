library(kernlab)
library(caret) #automatic parameter tuning by re-sampling. Very powerful, and handles lots of different model fits. Needs libraries: ggplot2, gtable, plyr, digest, proto, reshape2, stringr, scales, munsell, colorspace, dichromat, labeling, you get the idea, etc. etc. etc.
library(e1071)
library(randomForest)


train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)


polyModel <- ksvm(x=predictors,y=response,kernel='polydot',kpar=list(degree=5,scale=.001,offset=1), C=.5)
polyFiveDegreePredictions <- predict(polyModel,newdata=test)


tunecontrol  <- trainControl(method="cv",number=10)
randomForestModel <- train(x=predictors,y=response,method="rf",trcontrol=tunecontrol)
randomForestPredictions <- predict(randomForestModel,newdata=test)



svmRadialModel <- train(x=predictors,y=response,method='svmRadial',trcontrol=tunecontrol)
svmRadialPredictions <- predict(svmRadialModel,newdata=test)


polySixDegreeModel <- ksvm(x=predictors,y=response,kernel='polydot',kpar=list(degree=6,scale=.001,offset=1), C=.5)
polySixDegreePredictions <- predict(polySixDegreeModel,newdata=test)



boosting_gbm <- read.csv("C:/Users/JamesBladen/Desktop/stat 154 kaggle final/New folder/boosting_gbm.csv")
GBMPredictions <- boosting_gbm[,2]

elastic_model <- read.csv("C:/Users/JamesBladen/Desktop/stat 154 kaggle final/New folder/elastic_model.csv")
elasticPredictions <- elastic_model[,2]

original <- polyFiveDegreePredictions

polyFiveDegreePredictions[elasticPredictions==6] <- 6
polyFiveDegreePredictions[randomForestPredictions==1] <- 1
polyFiveDegreePredictions[elasticPredictions==5] <- 5
polyFiveDegreePredictions[elasticPredictions==4] <- 4
polyFiveDegreePredictions[polySixDegreePredictions==3] <- 3
polyFiveDegreePredictions[svmRadialPredictions==2] <- 2
polyFiveDegreePredictions[polySixDegreePredictions==8] <- 8
polyFiveDegreePredictions[GBMPredictions==7] <- 7







submissions = cbind((1:nrow(test)), polyFiveDegreePredictions)
write.table(submissions, file = "aggregateMethod.csv", sep = ",", col.names = c("Id", "Predictions"), row.names = F) #I had some trouble getting write.csv to output the submissions in the correct format for kaggle, so I just used write.table and specified a comma as separator.















polyFiveDegreePredictions   
randomForestPredictions    
svmRadialPredictions   
polySixDegreePredictions 
GBMPredictions  
elasticPredictions















