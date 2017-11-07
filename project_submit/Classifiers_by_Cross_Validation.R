#For the Airbnb dataset
#For predicting the destination of flights
#Using k - fold cross validation in this case
#Possible that it's a little slow depending on the operating speed of the system.


library(rpart)
library(e1071)
library(nnet)
library(adabag)
library(randomForest)
library(caret)
library(adabag)
library(plyr)
library(kernlab)
library(ipred)
library(gbm)


#R should be set to the specific working directory.
#Loading the data
#Current location of the Dataset
dataDirectory = ("C:\\Users\\Preethi\\Documents\\Spring 2017\\Machine Learning\\Project\\Final Project\\Dataset\\")
#reading the actual training data
airbnb_data <- read.csv(paste(dataDirectory, 'airbnb_dataset.csv', sep=""), header = TRUE)
#Making the final output to be a categorical value for our convenience
airbnb_data$country_destination <- as.factor(airbnb_data$country_destination)
#Performing k-fold validation
#Here k is 3
Training_rule <- trainControl(method="cv", number=3, returnResamp = "all", classProbs = TRUE)




#Algorithms
#Decision tree
#Fiding the accuracy and kappa values.
train_dtree <- train(country_destination ~ ., data = airbnb_data, method = "rpart", tuneLength = 9, trControl=Training_rule)
accr_dtree_cv <-  100*max((train_dtree$results$Accuracy))
kappa_dtree_cv <- 100*max((train_dtree$results$Kappa))
#For visualization purposes
plot(train_dtree)
#to know the accuracy value
accr_dtree_cv
#to know the kappa value
kappa_dtree_cv

#Neural Nnetwork
#Fiding the accuracy and kappa values.
train_nnet <- train(country_destination ~., data = airbnb_data, method = "nnet", trControl = Training_rule, preProc = c("center", "scale"), trace = FALSE)
accr_nnet_cv <- 100*max((train_nnet$results$Accuracy))
kappa_nnet_cv <- 100*max((train_nnet$results$Kappa))
#For visualization purposes
plot(train_nnet)
#to know the accuracy value
accr_nnet_cv
#to know the kappa value
kappa_nnet_cv



#SVM with radial kernel
#Fiding the accuracy and kappa values.
Grid_svm <- data.frame(.C = c(.25, .5, 1), .sigma = .05)
train_svm <- train(country_destination~ ., data=airbnb_data, method="svmRadial", trControl=Training_rule, tuneGrid=Grid_svm, preProc= c("center", "scale"))
accr_svm_cv <-  100*max((train_svm$results$Accuracy))
kappa_svm_cv <- 100*max((train_svm$results$Kappa))
#For visualization purposes
plot(train_svm)
#to know the accuracy value
accr_svm_cv
#to know the kappa value
kappa_svm_cv


#Bagging
#Fiding the accuracy and kappa values.
train_bag <- train(country_destination~ ., data=airbnb_data, method="treebag", trControl=Training_rule, nbagg = 7, keepX = TRUE)
accr_bag_cv <-  100*(train_bag$results$Accuracy)    
kappa_bag_cv <- 100*(train_bag$results$Kappa)
#to know the accuracy value
accr_bag_cv
#to know the kappa value
kappa_bag_cv

#Gradient Boosting
#Fiding the accuracy and kappa values.
train_gboost <- train(country_destination~ ., data=airbnb_data, method="gbm", trControl=Training_rule)
accr_gboost_cv <- 100*(max(train_gboost$results$Accuracy))
kappa_gboost_cv <- 100*(max(train_gboost$results$Kappa))
#to know the accuracy value
accr_gboost_cv
#to know the kappa value
kappa_gboost_cv

#Ada Boosting
#Fiding the accuracy and kappa values.
Grid_aboost <- expand.grid(maxdepth=25,mfinal=10, coeflearn="Breiman")
train_aboost <- train(country_destination~ ., data=airbnb_data, method = "AdaBoost.M1", trControl = Training_rule, tuneGrid=Grid_aboost)
accr_aboost_cv <- 100*(train_aboost$results$Accuracy)
kappa_aboost_cv <- 100*(train_aboost$results$Kappa)
#to know the accuracy value
accr_aboost_cv
#to know the kappa value
kappa_aboost_cv
