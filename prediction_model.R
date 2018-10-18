####
#### By Elim Thompson (10/14/2018)
####
#### This R script follows the Kaggle Lesson -
#### Machine Learning in R Level 1. This tutorial
#### covers the R package that trains data using
#### decision regression tree and random forest.
####
#### https://www.kaggle.com/learn/r
####
################################################

################################################
#### load packages
################################################
### load tidyverse package for basic functions
library (tidyverse)
### load rpart package for regression tree
##  rpart = Recursive Partitioning And Regression Trees
library (rpart)
### load randomForest for random forest
library (randomForest)
### load modelr for model accuracy / precision
library (modelr)

################################################
#### explore data
################################################
### read in Iowa house data in CSV format
iowa_data <- read_csv ('/home/elims/atom/learnR/kaggle_BDT/data/iowa/train.csv')
### show summaries for all columns
summary (iowa_data)
### look at the sorted available column Names
sort (names (iowa_data))
### fit a regression tree via rpart
##  target_variable = house sale price
##  predictors = factors to predict target_variable
##  Syntax: rpart (target_variable ~ predictor1 + predictor2, data=data_frame)
fit <- rpart (SalePrice ~ BedroomAbvGr + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + YearBuilt, data=iowa_data)
### show the regreession tree in diagram
### via rpart::plot and rpart::text
plot (fit, uniform=TRUE); text (fit, cex=.6)

################################################
#### make prediction from fitted regression tree
################################################
### make prediction on the first 6 rows fom the training
### set via rpart::predict
predicted <- predict (fit, head (iowa_data))
### calculate mean absolute errors (MAE)
##  error = abs (actual - predicted)
actual <- iowa_data$SalePrice
predicted <- predict (fit, iowa_data)
print (mean (abs (actual - predicted)))
##  same averaged error can be calculated via modelr::mae ()
print (mae (model=fit, data=iowa_data))

################################################
#### split data to prevent sample bias
################################################
### split data into training (0.7) and testing (0.3)
### via modelr::resample_partition ()
splitData <- resample_partition (iowa_data, c(test=0.3, train=0.7))
### show how many rows in each train and test subsamples
##  both have 81 columns
print (lapply (splitData, dim))
### fit with just training set
fit2 <- rpart (SalePrice ~ BedroomAbvGr + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + YearBuilt, data=splitData$train)
### get modelr::mae ()
print (mae (model=fit2, data = splitData$test))

################################################
#### playing with rpart options
####
#### A good prediction model has a minimum MAE
#### from the testing (validation) set among the
#### different tree settings (e.g. maximum depth
#### of the tree).
################################################
### define a function (like above codes) to
### return mae value for a given maxdepth
get_mae <- function (maxdepth, target, predictors, trainset, testset) {
  ## turn the predictors into a collapsed long string
  predictors <- paste (predictors, collapse="+")
  ## put together the target_variable and predictors as a formula
  formula <- as.formula (paste (target, "~", predictors, sep=""))
  ## fit the model using train set with the given maxdepth
  model <- rpart (formula, data=trainset,
                  control=rpart.control (maxdepth=maxdepth))
  ## calculate mae error
  maerror <- mae (model, testset)
  ## return mae error
  return (maerror)
}
### define variables for regression Tree
target <- "SalePrice"
predictors <- c("BedroomAbvGr", "BsmtFullBath", "BsmtHalfBath",
                "FullBath", "HalfBath", "YearBuilt")
### loop through each desire maxdepth
for (i in 1:10) {
  maerror <- get_mae (maxdepth=i, target=target, predictors=predictors,
                      trainset=splitData$train, testset=splitData$test)
  print (glue::glue ("Maxdepth: ", i, "\t MAE: ", maerror))
}
### In this case, the minimum mae happens at maxdepth = 5.
### This is the optimized maxdepth. Other stopping criteria
### (e.g. no more events in the nodes) prevents more depth
### which is why the mae stays the same after maxdepth = 5.

################################################
#### Random forest instead of regression tree
####
#### One tree can easily lead to under- or over-
#### training. One clever way to avoid bad
#### modeling is random forest, which uses
#### many trees. The final result is the average
#### from all trees.
################################################
### fit a random forest model to training set
### via randomForest ()
fitRandomForest <- randomForest (SalePrice ~ BedroomAbvGr + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + YearBuilt, data=splitData$train)
### get the mae from test data
print (mae (model=fitRandomForest, data=splitData$test))
