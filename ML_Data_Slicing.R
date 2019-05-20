# Data Slicing
# Data slicing functions return indices
# createDataPartition(), createFolds(), createResample()

## Load library and data
library(caret); library(kernlab); data(spam)

## Data splitting
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
trainig <- spam[inTrain, ]
testing <- spam[-inTrain,]
training
dim(training)
dim(testing)

## K-fold
set.seeds(32323)
folds_train <- createFolds(y = spam$type,
                     k = 10,
                     list = TRUE,
                     returnTrain = TRUE)
sapply(folds_train, length)

set.seeds(32323)
folds_test <- createFolds(y = spam$type,
                     k = 10,
                     list = TRUE,
                     returnTrain = FALSE)
sapply(folds_test, length)
folds[[1]][1:10]
spam[folds_test[[1]],]


## Resampling
set.seed(32323)
resampling <- createResample(y = spam$type,
                             times = 10,
                             list = TRUE)
sapply(resampling, length)
resampling[[1]][1:5]
spam[resampling[[1]][1:5], ]


## Time Slices
set.seed(32323)
tme <- 1:1000
timeslice <- createTimeSlices(y = tme,
                              initialWindow = 20,
                              horizon = 10)
names(timeslice)
str(timeslice$train)
timeslice$train$Training020
timeslice$test$Testing020
timeslice$train$Training021
timeslice$test$Testing021
timeslice$train$Training990
timeslice$test$Testing990
