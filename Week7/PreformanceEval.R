rm(list = ls())
graphics.off()
cat("\014") 
library(caret)
library(plyr)
library(ggplot2)
set.seed(123)

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week7/data')
ab = read.csv('abalone.data', header = FALSE)
names(ab) = c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", 
              "viscera_weight", "shell_weight", "rings")


ab1 = ab[c(9)]
ab= ab[-c(9)]
ab = transform(ab, volume = length * diameter * height)
ab = transform(ab, mweight = whole_weight - shucked_weight)
ab1 = transform(ab1, year = rings + 1.5)
ab$year = ab1$year
ab$sex = revalue(ab$sex, c("M"=1,"F"=2,"I"=3))

#Quick EDA
qplot(shell_weight, year, data=ab, geom=c("point", "smooth"), method="lm")
qplot(shell_weight, year, data=ab, geom=c("point", "smooth"), method="lm",
      color=sex) 
qplot(sex, year, data=ab, geom=c("point", "smooth"), method="lm",
      color=sex) 


# Lets see if we can do better trying to predict the SEX of the 
control1 = trainControl(method="repeatedcv", number=10, repeats=2)
control = trainControl(method="adaptive_cv", number=10, repeats=2)
metric = "Accuracy"

# Test Metric
# 
# Some good test metrics to use for different problem types include:
#   
#   Classification:
#   
# * Accuracy: x correct divided by y total instances. Easy to understand and widely used.
# * Kappa: easily understood as accuracy that takes the base distribution of classes into account.
# 
# Regression:
#   
#   * RMSE: root mean squared error. Again, easy to understand and widely used.
#   * Rsquared: the goodness of fit or coefficient of determination.
# 
#  ROC and LogLoss are other popular measures.


# Predicting Age/Ring/Year(ring + 1.5)
fit.knn = train(year~., data = ab, method= "knn",  preProcess="scale",trCrontrol = control)
fit.rf = train(year~., data=ab, method="rf", preProcess="scale", trCrontrol = control)
#fit.svm = train(year~., data=ab, method="svmRadial", preProcess="scale", trControl=control1)
fit.cart = train(year~., data=ab, method="rpart", preProcess="scale", trControl=control1)
results = resamples(list(knn=fit.knn, rf=fit.rf, cart=fit.cart))
#results = resamples(list(knn=fit.knn, rf=fit.rf, svm=fit.svm, cart=fit.cart))
summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)
summary(results)

#Predicting Sex
fit.knn2 = train(sex~., data=ab, method="knn", preProcess="scale", trControl=control)
fit.rpart2 = train(sex~., data=ab, method="rpart", preProcess="scale", trControl=control1)
# Take a lot of computing power to finish but ends up as #2 
fit.rf2 = train(sex~., data=ab, method="rf", preProcess="scale", trControl=control1)
fit.svm2 = train(sex~., data=ab, method="svmRadial", preProcess="scale", trControl=control1)
fit.cart2 = train(sex~., data=ab, method="rpart", preProcess="scale", trControl=control1)

results2 = resamples(list(knn=fit.knn2, rpart=fit.rpart2, rf=fit.rf2, svm=fit.svm2, cart=fit.cart2))
bwplot(results2)
dotplot(results2)
summary(results2)

