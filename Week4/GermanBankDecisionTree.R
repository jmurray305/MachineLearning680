rm(list = ls()) 
graphics.off()
cat("\014") 

#Libraries 
library(rpart)
library(caret)
library(randomForest)

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week4/data')
bank = read.csv('german.data', sep = " ", header = FALSE)

# Cost Matrix (Good_Bad) column 1=Good 2=Bad
names(bank) = c('status','duration','credit_history','purpose','credit_amount','saving_account','empolyment',
                'install_rate','sex_status','other_debtors','residence','property','age','other_plans','housing',
                 'num_credits','job','liable_amount','telephone','foreign_worker', 'Good_Bad' )

str(bank)
bank$Good_Bad = as.factor(bank$Good_Bad)

#Split data into 70-30 train-test sets
set.seed(23)
ind = sample(2, nrow(bank), replace = TRUE , prob = c(0.7,0.3))
trainset = bank[ind == 1, ]
testset = bank[ind == 2, ]

#Building model to predict class, first parameter is the column you want to predict not the dataset
bank.rp = rpart(Good_Bad ~ ., data=trainset)
bank.rp
printcp(bank.rp)
plotcp(bank.rp)
summary(bank.rp)

#plotting decision tree
plot(bank.rp, uniform=TRUE, branch =0.25, margin = 0.125);text(bank.rp, all=TRUE, use.n = TRUE)

#predicting Good or Bad
predictions = predict(bank.rp, testset, type = "class")
table(testset$Good_Bad, predictions, dnn= c("actual","predicted"))
confusionMatrix(table(testset$Good_Bad, predictions, dnn= c("actual","predicted")))

#Prune Tree
min(bank.rp$cptable[,"xerror"])
which.min(bank.rp$cptable[,"xerror"])
bank.cp = bank.rp$cptable[4,"CP"]
prune.tree = prune(bank.rp, cp = bank.cp)


#plot the pruned tree
plot(prune.tree, uniform=TRUE, branch =0.6, margin = 0.125);text(prune.tree, all=TRUE, use.n = TRUE)

#predict again with the prune settings
prediction2 = predict(prune.tree, testset, type = "class")
table(testset$Good_Bad, prediction2)
confusionMatrix(table(testset$Good_Bad, prediction2))

#Random Forest Time
rf = randomForest(Good_Bad ~ .,   data=trainset)
print(rf)      # view results
importance(rf) # importance of each predictor 
rfprediction = predict(rf, newdata = testset)
table(rfprediction, testset$Good_Bad)
confusionMatrix(table(rfprediction, testset$Good_Bad))

