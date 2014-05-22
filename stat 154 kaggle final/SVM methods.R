library(kernlab)
library(caret) #automatic parameter tuning by re-sampling. Very powerful, and handles lots of different model fits. Needs libraries: ggplot2, gtable, plyr, digest, proto, reshape2, stringr, scales, munsell, colorspace, dichromat, labeling, you get the idea, etc. etc. etc.
library(e1071)



train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)








polynomialSVM <- train(x=predictors,y=response,method = 'svmPoly')

testPrediction <- predict(polynomialSVM$finalModel,newdata=test)

mean(testPrediction==submission4_26_SVM_2[,2])






getwd()
submissions = cbind((1:nrow(test)), testPrediction)
write.table(submissions, file = "svmPolypredictions.csv", sep = ",", col.names = c("Id", "Predictions"), row.names = F) #I had some trouble getting write.csv to output the submissions in the correct format for kaggle, so I just used write.table and specified a comma as separator.



