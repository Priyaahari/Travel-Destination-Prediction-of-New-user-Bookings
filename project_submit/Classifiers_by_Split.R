#For Airbnb dataset
#For predicting the destination of the passengers
#Using split method
#Split being 80-20(training-test)
#might take a little while depending on the processing speed of the system


library(rpart)
library(e1071)
library(nnet)
library(adabag)
library(randomForest)
library(plyr)
library(rpart.plot)

#R should be set to the specific working directory.
#Loading the data
#Current location of the Dataset
dataDirectory = ("C:\\Users\\Preethi\\Documents\\Spring 2017\\Machine Learning\\Project\\Final Project\\Dataset\\")
#reading the training data
airbnb_data <- data.frame(read.csv(paste(dataDirectory, 'airbnb_dataset.csv', sep=""), header = TRUE))
#Making the final output to be a categorical value for our convenience
airbnb_data$country_destination <- as.factor(airbnb_data$country_destination)
#Using the 80-20 split for getting the training and test set
sample_set <- sample(nrow(airbnb_data), floor(nrow(airbnb_data) * 0.8))
train_set <- airbnb_data[sample_set, ]
test_set <- airbnb_data[-sample_set, ]

#Algorithms
#Decision Tree
#Finding Accuracy
train_dtree <- rpart(country_destination ~ ., data = train_set, method = 'class', parms = list(split ='information'))
pruned_dtree <- prune(train_dtree, cp = 0.01)
prediction_dtree <- predict(pruned_dtree, test_set, type = "class")
#Calculating accuracy
dtree_accuracy <- 100*sum(prediction_dtree == test_set$country_destination)/length(prediction_dtree)
#For vizualization purposes
rpart.plot(pruned_dtree)
#Accuracy
dtree_accuracy

#Perceptron
train_perceptron <- nnet(country_destination~., train_set, size=0, maxit=1000, na.action = na.omit, skip = T , MaxNWts = 30000)
prediction_perceptron <- predict(train_perceptron, test_set, type = "class")
#Calculating accuracy
nn_accuracy <- 100*sum(prediction_perceptron == test_set$country_destination)/length(prediction_perceptron)
#Accuracy
nn_accuracy


#Neural networks
train_nn <- nnet(country_destination~. , train_set, size = 20, maxit = 1000, na.action = na.omit , skip = F , MaxNWts = 30000)
prediction_nn <- predict(train_nn, test_set , type = "class")
#Calculating accuracy
ann_accuracy <- 100*sum(prediction_nn == test_set$country_destination)/length(prediction_nn)
#Accuracy
ann_accuracy

#SVM
train_svm <- svm(country_destination~., data = train_set, scale = FALSE , kernel = "polynomial")
prediction_svm <- predict(train_svm, test_set, type = C)
#Calculating accuracy
svm_accuracy <- 100*sum(prediction_svm == test_set$country_destination)/length(prediction_svm)
#Accuracy
svm_accuracy

#Naive Bayes
train_nb <- naiveBayes(country_destination~., data = train_set)
prediction_nb <- predict(train_nb, test_set)
#Calculating accuracy
nb_accuracy <- 100*sum(prediction_nb == test_set$country_destination)/length(prediction_nb)
#Accuracy
nb_accuracy

#Bagging
train_bag <- bagging(country_destination~.,data = train_set, mfinal = 10)
prediction_bag <- predict(train_bag , test_set)
#Calculating accuracy
bagging_accuracy <- 100 - (prediction_bag$error)*100
#Finding Accuracy
bagging_accuracy

#Ada Boosting
train_adaboost <- boosting(country_destination~., data=train_set)
prediction_adaboost <- predict.boosting(train_adaboost , test_set)
#Calculating accuracy
adaboost_accuracy <- 100 - (prediction_adaboost$error)*100
#Accuracy
adaboost_accuracy