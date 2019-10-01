rm(list = ls()) 
graphics.off()
cat("\014")  
library(ggplot2)
library(class)
library(reshape)
library(Rfast)
library(caret)
library(dplyr)

set.seed(1999) # For academic purposes when using a pseudorandom function and party like its 1999
MyHeart = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", header = FALSE, sep= ',')

colnames(MyHeart) = make.names(c("Age","Sex","CP","Trestbps","Chol","FBS","Restecg","Thalach","Exang","Oldpeak","Slope","ca","thal","num")) 
str(MyHeart)
MyHeart[,'num']=factor(MyHeart[,'num'])
MyHeart$ca = as.numeric(MyHeart$ca)
MyHeart$thal = as.numeric(MyHeart$thal)

normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Heart2 = as.data.frame(lapply(MyHeart[,c(1,2,3,4,5,6,7,8,9,10,11,12,13)], normalize))
str(Heart2)

Heart2$Set <- sample(c("training", "test"), nrow(MyHeart), replace = TRUE, prob = c(0.8, 0.2))

head(MyHeart$Set, n = 10)

MyHeartLevels <- levels(MyHeart$num) # Might what this later to convert numeric Sex back to factor or character
MyHeart$num <- as.numeric(MyHeart$num)
Heart2$Target = MyHeart$num


HTraining <- subset(Heart2, Set == "training", select = 1 : 13)
HTest <- subset(Heart2, Set == "test", select = 1 : 13)
HTrain_target = subset(Heart2, Set == "training", select = 15)
HTest_target = subset(Heart2, Set == "test", select = 15)
nrow(HTraining) + nrow(HTest)


HTrain_target <- factor(HTrain_target$Target)
HTest_target <- factor(HTest_target$Target)

MyPred = class::knn(HTraining, HTest, cl=HTrain_target, k=17)
Actual = HTest_target
MyConfusionMatrix <- table(Actual, MyPred)
MyConfusionMatrix
caret::confusionMatrix(MyConfusionMatrix)






