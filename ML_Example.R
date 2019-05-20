# Macine Learning Practice


## Install caret package
if(!require("caret")) install.packages("caret", dependencies=TRUE)
if(!require("e1071")) install.packages('e1071', dependencies=TRUE)
library(caret)


## Install data package
if(!require("kernlab")) install.packages("kernlab")
library(kernlab)


## Data splitting
data(spam)

inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

## Fit a model

set.seed(32343)
modelFit <- train(type ~ ., 
                  data = training,
                  method = 'glm')
modelFit

## Final Model
modelFit$finalModel

## Prediction
predictions <- predict(modelFit, newdata = testing)
predictions

## Check predictions - confusion matrix
confusionMatrix(predictions, testing$type)
