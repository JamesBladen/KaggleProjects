train <- read.csv("C:/Users/JamesBladen/Desktop/train.csv", header=F)
test <- read.csv("C:/Users/JamesBladen/Desktop/test.csv", header=F)
library(randomForest)
library(class)
library(rpart)

predictors <- as.matrix(train[,-1])
response <- as.factor(train[,1])
colnames(predictors) <- colnames(test)

rf <- randomForest(x=predictors,y=response,ntree=1000)
randomForestPredictions <- predict(rf,as.matrix(test))


setwd("C:/Users/JamesBladen/Desktop")
submissions = cbind((1:nrow(test)), randomForestPredictions)
write.table(submissions, file = "randomForest.csv", sep = ",", col.names = c("Id", "Predictions"), row.names = F) #I had some trouble getting write.csv to output the submissions in the correct format for kaggle, so I just used write.table and specified a comma as separator.






tree <- rpart(response~predictors, control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))
plotcp(tree)
tree.pruned = prune(tree, cp = tree$cptable[13,1])

plot(tree.pruned)
text(tree.pruned)
table(predict(tree.pruned,data.frame(predictors),type="class"),response)
predict(tree.pruned,test[1:800,],type="class")




###################################ALREADY ATTEMPTED, got .68
names(tree.pruned$variable.importance)
importantVariables <- c(138,12,266,124,140,10,156,252,28,154,97,113,476,282,250,225,326,348,33,17,81,7,332,23,119,220,204,328,135,458,454,151,247,198,330,298,170,314,195,426,322,442,234,396,197,391,323,268,69,181,201,213,273,390,202,451,193,171,327,450,152,168,264,280,133,1,129,145,331,455,100,40,51,219,43,389,5,498,47,59,91,9,39,35,67,274,130,131,146,259,194,311,66,70,74)
rf <- randomForest(x=predictors[,importantVariables],y=response,ntree=1000)
randomForestPredictions <- predict(rf,as.matrix(test[,importantVariables]))

setwd("C:/Users/JamesBladen/Desktop")
submissions = cbind((1:nrow(test)), randomForestPredictions)
write.table(submissions, file = "randomForest.csv", sep = ",", col.names = c("Id", "Predictions"), row.names = F) #I had some trouble getting write.csv to output the submissions in the correct format for kaggle, so I just used write.table and specified a comma as separator.




