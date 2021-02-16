library(mlr3verse)
library(ggplot2)
set.seed(1234)


# task --------------------------------------------------------------------

# initialize task, if personalized = TRUE all variables are used, 
# otherwise "george", "hp", "num650" are removed
task <- tsk("spam")

personalized <- FALSE

if (!personalized) {
  keep <- setdiff(task$feature_names, c("george", "hp", "num650"))
  task$select(keep)
}




# xgboost

lrn_xgb = lrn("classif.xgboost", predict_type = "prob")

# Define the ParamSet
param_xgb = ParamSet$new(
  params = list(
    ParamDbl$new(id = "eta", lower = 0.2, upper = .4),
    ParamDbl$new(id = "min_child_weight", lower = 1L, upper = 20L),
    ParamDbl$new(id = "subsample", lower = .7, upper = .8),
    ParamDbl$new(id = "colsample_bytree",  lower = .9, upper = 1),
    ParamDbl$new(id = "colsample_bylevel", lower = .5, upper = .7),
    ParamInt$new(id = "nrounds", lower = 1L, upper = 200L)
  ))



tnr_xgb = AutoTuner$new(
  learner = lrn_xgb,
  resampling = rsmp("cv", folds = 5L),
  measure = msr("classif.auc", id = "AUC"),
  search_space = param_xgb, 
  terminator = trm("evals", n_evals = 200L),
  tuner = tnr("grid_search"),
  store_models = TRUE)


#tune
future::plan("multisession")

start <- Sys.time()
tnr_xgb$train(task)
print(Sys.time()-start)


save(tnr_xgb, file = paste0("benchmarks/tnr_xgb", as.Date(Sys.time()), ".RData"))


param_best_xgb <- tnr_xgb$tuning_result
param <- unlist(param_best_xgb$learner_param_vals)
