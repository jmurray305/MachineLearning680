rm(list = ls()) 
graphics.off()
cat("\014")  
library(ggplot2)
library(class)
library(reshape)
library(Rfast)
library(caret)
require(plyr)

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week2/')
MyBank = read.csv("bank-full.csv", sep = ";")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normBalance <- as.data.frame(lapply(MyBank[6], normalize))
MyBank$normalBalance = normBalance$balance
str(MyBank)

MyBank$month = revalue(x = MyBank$month, c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4 ,"may" = 5 , "jun" = 6,
                                            "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12))
MyBank$month = as.numeric(MyBank$month)
MyBank$loan = revalue(x = MyBank$loan, c("no" = 0, "yes"= 1))
MyBank$loan = as.numeric(MyBank$loan)
MyBank$default = revalue(x = MyBank$default, c("no" = 0, "yes"= 1))
MyBank$default = as.numeric(MyBank$default)

header = c("age","default","loan","day","campaign","pdays","previous","month","normalBalance","term")
BankNew <- MyBank[header]
str(BankNew)


MyBankMelt <- melt(BankNew, id.vars = "term")

ggplot(MyBankMelt, aes(x = variable, y = value)) + 
  geom_boxplot()


set.seed(1999) # For academic purposes when using a pseudorandom function
BankNew$Set <- sample(c("training", "test"), nrow(BankNew), replace = TRUE, prob = c(0.8, 0.2)) 
head(BankNew$Set, n = 10)

MyBankLevels <- levels(BankNew$term) # Might what this later to convert numeric Sex back to factor or character
BankNew$term <- as.numeric(BankNew$term)

BTraining <- subset(BankNew, Set == "training", select = 1 : 10)
BTest <- subset(BankNew, Set == "test", select = 1 : 10)
nrow(BTraining) + nrow(BTest) # Should be 45211

cl = BTraining[,10]
MyPred <- class::knn(BTraining, BTest,cl=BTraining$term,k=5) # Note that all columns must be numeric
Actual <- BTest$term
MyConfusionMatrix <- table(Actual, MyPred)
MyConfusionMatrix
caret::confusionMatrix(MyConfusionMatrix)


MyPred <- class::knn(BTraining, BTest,cl=BTraining$term,k=7) # Note that all columns must be numeric
Actual <- BTest$term
MyConfusionMatrix <- table(Actual, MyPred)
MyConfusionMatrix
caret::confusionMatrix(MyConfusionMatrix)


MyPred <- class::knn(BTraining, BTest,cl=BTraining$term,k=11) # Note that all columns must be numeric
Actual <- BTest$term
MyConfusionMatrix <- table(Actual, MyPred)
MyConfusionMatrix
caret::confusionMatrix(MyConfusionMatrix)


MyPred <- class::knn(BTraining, BTest,cl=BTraining$term,k=3) # Note that all columns must be numeric
Actual <- BTest$term
MyConfusionMatrix <- table(Actual, MyPred)
MyConfusionMatrix
caret::confusionMatrix(MyConfusionMatrix)

