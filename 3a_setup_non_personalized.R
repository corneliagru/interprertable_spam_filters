library(iml)
library(mlr3verse)
library(ggplot2)
library(ranger)
library(e1071)
library(data.table)

set.seed(1234)

#initialize task and learners
source('2a_setup_learners.R')

#function to access predictions from ranger
pfun <- function(object, newdata) predict(object, data = newdata)$predictions 
psvm <- function(object, newdata) attr(predict(object, newdata = newdata, probability = TRUE),"probabilities")[,1]
auc_error <- function(actual, predicted) 1 - Metrics::auc(actual, predicted)




task_np <- tsk("spam")
task_np$id <- "spam_np"
#remove personalized words
keep <- setdiff(task_np$feature_names, c("george", "hp", "num650"))
task_np$select(keep)


spam_np <- task_np$data()
spam_np <- as.data.frame(spam_np)


#models
lrn_ranger_tuned$train(task_np)
lrn_svm_tuned$train(task_np)


rng_np <- lrn_ranger_tuned$model 
svm_np <- lrn_svm_tuned$model

pred_np = Predictor$new(rng_np,
                          data = spam_np[-which(names(spam_np) == "type")],
                          y = (spam_np$type == "spam"),
                          predict.fun = pfun,
                          class = "spam")

