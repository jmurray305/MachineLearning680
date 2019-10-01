rm(list = ls()) 
graphics.off()
cat("\014") 

#Libraries 
library(rpart)
library(caret)
library(randomForest)

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week4/')
wine = read.csv('wine.data', header = FALSE)
names(wine) = c("class", "alcohol", "malic_acid", "ash", "alcalinity_of_ash", "magnesium", "total_phenols", "flavanoids",
                 "nonflavanoid_phenols", "proanthocyanins", "color_intensity", "hue", "OD280_OD315", "proline")

#wine= data.frame(wine)
str(wine)
wine$class = as.factor(wine$class)
#Split 70% of the data into training and 30% into testing dataset
set.seed(13)
ind = sample(2, nrow(wine), replace = TRUE , prob = c(0.7,0.3))

trainset = wine[ind == 1, ]
testset = wine[ind == 2, ]

#explore dimensions in the training and testing datasets
dim(trainset)
dim(testset)

#Building model to predict class, first parameter is the column you want to predict not the dataset
wine.rp = rpart(class ~ ., data=trainset)
wine.rp
printcp(wine.rp)
plotcp(wine.rp)
summary(wine.rp)

#plotting decision tree
plot(wine.rp, uniform=TRUE, branch =0.6, margin = 0.125);text(wine.rp, all=TRUE, use.n = TRUE)

#Measuring the prediction preformance
##I was getting and extra prediction of 1.42857142857143 in three instances (2 predicted as class on and 1 correctly redicted as 1.42....) 
##I dont know why i was getting these 'new' predicted points but to fix it i converted the class column into a factor and used 
## the type 'class' to get the prediction working correctly.

predictions = predict(wine.rp, testset, type = "class")
table(testset$class, predictions, dnn= c("actual","predicted"))

confusionMatrix(table(testset$class, predictions))
# using a basic decision tree we are getting a accuracy of 88.76% lets see if we can improve that by pruning this tree.

#Pruning the tree
min(wine.rp$cptable[,"xerror"])
which.min(wine.rp$cptable[,"xerror"])
wine.cp = wine.rp$cptable[4,"CP"]
prune.tree = prune(wine.rp, cp = wine.cp)

#plot the pruned tree
plot(prune.tree, uniform=TRUE, branch =0.6, margin = 0.125);text(prune.tree, all=TRUE, use.n = TRUE)

#predict again with the prune settings
predictions = predict(prune.tree, testset, type = "class")
table(testset$class, predictions)
confusionMatrix(table(testset$class, predictions))

## it looks like pruning the tree did not increase nor decrease the tress's preformance. Lets see if a Random Forest will 
# increase the algorithms preformance. 


# Random Forest prediction of wine class
rf = randomForest(class ~ .,   data=trainset)
print(rf)      # view results
importance(rf) # importance of each predictor 
rfprediction = predict(rf, newdata = testset)
table(rfprediction, testset$class)
confusionMatrix(table(rfprediction, testset$class))
#with a random forest we get an accuracy of 100%
