# always run this file, work on specific task then

library(iml)
library(mlr3verse)
library(ggplot2)
library(ranger)
library(e1071)
library(data.table)
library("future")
library("future.callr")


set.seed(1234)

#initialize task and learners
source('2a_setup_learners.R')

#function to access predictions from ranger
pfun <- function(object, newdata) predict(object, data = newdata)$predictions 
psvm <- function(object, newdata) attr(predict(object, newdata = newdata, probability = TRUE),"probabilities")[,1]
auc_error <- function(actual, predicted) 1 - Metrics::auc(actual, predicted)


task <- tsk("spam")
spam <- task$data()
spam <- as.data.frame(spam)

#models
lrn_ranger_tuned$train(task)
#lrn_xgb_tuned$train(task)
lrn_svm_tuned$train(task)


rng <- lrn_ranger_tuned$model 
svm <- lrn_svm_tuned$model
#xgb <- lrn_xgb_tuned$model


predictor = Predictor$new(rng,
                          data = spam[-which(names(spam) == "type")],
                          y = (spam$type == "spam"),
                          predict.fun = pfun,
                          class = "spam")

