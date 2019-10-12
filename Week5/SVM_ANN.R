rm(list = ls()) 
graphics.off()
cat("\014") 

# ANN
# Artificial Neural Network (ANN or NN) is an information processing modeld inspired by the biological neuron system that has the 
# ablitly to learn by examples. It is made up of a massive number of highly interconnected processing elements know as neurons 
# that are used to solve problems. In the learning phase, the network learns by adjusting the weights to predict the correct
# class label go a given input. The Neural network is made up of 3 types of connected nodes (input, hidden, and output).The connection
# strength between neurones is called weights. There are two types of weights, greater than zero(excitation status) less than 
# zero (inhibition). The major advantages of a NN are it can find non-linear relationships in independent and dependent variables, 
# can train large datasets efficiently and its a nonparametric model so that one drop errors in the estimation of parameters.
# However the main con of NN's are it often gets stuck in local minimums rather that the global and it may overfit if left 
# training too long. Activation functions  of a node defines the output of that node given an input or set of inputs. 
# Topology of a neural network refers to the way the nodes are connected and is an import factor in network functioning and learning
# Once the netwrok is created backpropagation is used as the main tool in which the NN learnings. Its the device that tells
# the network  wheather or not the net made a mistake when making a prediction. While a NN propagates the signal of the input data forward
# through the  network, backpropagation informates the network about the error in reverse through the network. Here is a breakdown on how it works
# * The network makes a guess about the data, using its nodes
# * The network is measured with a loss function
# * The error is backpropagated to adjuct the wrong-headed nodes
# Backpropagation takes the error with the wrong guess and uses that error to adjust the networks parameters in the direction of less error.
#
#

library(neuralnet)
library(e1071)
library(caret)

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week5/data')
mush = read.csv('mushrooms.csv')
str(mush)
colSums(is.na(mush))

#droping Veil Type (only has one type as 'partial')
mush = data.frame(mush[, -16])

# mush2 = as.data.frame(unclass(mush))
# convert = sapply(mush,is.factor)
# mush2=sapply(mush[,convert], unclass)
# mush1 = cbind(mush2[, !convert], mush2)
# mush1 = data.frame(mush1)




#mush2name = c("bruises", "gill.attachment", "gill.spacing", "gill.size", "stalk.shape", "class")
#mush2 = mush[mush2name]
#str(mush2)

#Split data into Test and Train
set.seed(13)
ind = sample(2, nrow(mush), replace = TRUE , prob = c(0.7,0.3))
trainset = mush[ind == 1, ]
testset = mush[ind == 2, ]


#SVM
# Support Vector Maching (SVM) is used to train a support vector machines can be used to carry out general regression and classification
# as well as density-estimation. SVMs is a discriminative classifier defined by a hyperplance that seperates data points. 
# If the data clustered close together we can use the kernel trick by applying a transformation and add one more dimension so the SVM
# can create a hyperplane the sperates the datapoints.

svm.model = svm(class ~ ., data = trainset, kernel = "linear", cost=1, gamma = 1/ncol(trainset))
svm.model2 = svm(class ~ ., data = trainset, kernel = "radial" , cost=1, gamma = 1/ncol(trainset))
svm.model3 = svm(class ~ ., data = trainset, kernel = "polynomial" , cost=1, gamma = 1/ncol(trainset))
summary(svm.model)
svm.pred = predict(svm.model, testset[, !names(testset) %in% c("class")])
svm.pred2 = predict(svm.model2, testset[, !names(testset) %in% c("class")])
svm.pred3 = predict(svm.model3, testset[, !names(testset) %in% c("class")])
svm.table = table(svm.pred, testset$class)
svm.table2 = table(svm.pred2, testset$class)
svm.table3 = table(svm.pred3, testset$class)

svm.table
svm.table2
svm.table3
classAgreement(svm.table)
#Linear acc = 100
confusionMatrix(svm.table)
#radial acc = 99.84
confusionMatrix(svm.table2)
#polynomial acc = 99.88
confusionMatrix(svm.table3)

#ANN

mush2 = as.data.frame(unclass(mush))
convert = sapply(mush,is.factor)
mush2=sapply(mush[,convert], unclass)
mush2 = cbind(mush2[, !convert], mush2)
mush2 = data.frame(mush2)

#Training and Test
ind = sample(2, nrow(mush2), replace = TRUE , prob = c(0.7,0.3))
trainset2 = mush2[ind == 1, ]
testset2 = mush2[ind == 2, ]


# train neutal network
mush2$class = as.factor(mush2$class)
str(mush2)
nn=neuralnet(class~.,data=mush2, hidden = 3, act.fct = "logistic", linear.output = FALSE)
# nn2=neuralnet(class~.,data=mush2, hidden = 3, act.fct = "logistic", linear.output = FALSE, 
              algorithm = "backprop", learningrate = 5)
#nn2
nn$result.matrix
#nn2$result.matrix
head(nn$generalized.weights[[1]])

#Visualizing the NN
plot(nn)
#plot(nn2)

#Predict
head(pred$net.result)
pred = predict(nn,testset2)
#pred2 = predict(nn2,testset2)
table(testset2$class == 1, pred[,1] > 0.5)
#table(testset2$class == 1, pred2[,1] > 0.5)
confusionMatrix(table(testset2$class == 1, pred[,1] > 0.5))
#confusionMatrix(table(testset2$class == 1, pred2[,1] > 0.5))


# 97.06% Accuracy for ANN.


# Conclusion
# ANN vs SVM - while these algorithms share the same concept of using linear learning model for patter recognition.
# The main difference is how they classifly  non-linear data. SVM utilizes nonliear mapping to make the data separable 
# (Keneral is Key) ANN has a tond of hidden layers with size h1 through hn depending on the number of features, plus bias paramters
# While SVM consists of a set of support vectors, selected  from the training set with a weight for each. One obvious
# advantage of ANN  over SVMs is that ANN may have any number of outputs, while SVMs have only one.
#
#
#
#
#

