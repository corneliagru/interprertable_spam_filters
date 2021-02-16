# Setup
library(mlr3verse)
library(ggplot2)
set.seed(1234)
theme_set(theme_minimal())


# task --------------------------------------------------------------------
# initialize task with data
task <- tsk("spam")
task$positive
spam <- task$data()
spam <- as.data.frame(spam)



# tuning ------------------------------------------------------------------
#ranger
lrn_ranger_tuned = lrn("classif.ranger",
                       predict_type = "prob",
                       importance = "impurity",
                       mtry = 7)


#xgboost
lrn_xgb_tuned = lrn("classif.xgboost",
                    predict_type = "prob",
                    verbose = 0,
                    nrounds = 67,
                    eta = 0.3333333,
                    min_child_weight = 1,
                    subsample = 0.7777778,
                    colsample_bytree = 0.9333333,
                    colsample_bylevel = 0.5)



#SVM

# use results from tuning
lrn_svm_tuned <- lrn("classif.svm",
                     predict_type = "prob",
                     type = "C-classification",
                     kernel = "radial",
                     cost = 13.33,
                     gamma = 0.012)
