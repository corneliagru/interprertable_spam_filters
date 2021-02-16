library(mlr3verse)
library(ggplot2)
set.seed(1234)

#personalized mtry = 7
#non personalized mtry = 6

# task --------------------------------------------------------------------

# initialize task, if personalized = TRUE all variables are used, 
# otherwise "george", "hp", "num650" are removed
task <- tsk("spam")

personalized <- TRUE


if (!personalized) {
  keep <- setdiff(task$feature_names, c("george", "hp", "num650"))
  task$select(keep)
}


ncol <- length(task$feature_names)
#Random forest

# initialize learner
lrn_ranger = lrn("classif.ranger",
                 predict_type = "prob",
                 importance = "impurity")

# initialize search/parameter space
param_mtry = ParamSet$new(list(
  ParamInt$new("mtry", lower = 2L, upper = 50L)))



tnr_ranger = AutoTuner$new(
  learner = lrn_ranger,
  resampling = rsmp("cv", folds = 5L),
  measure = msr("classif.auc", id = "AUC"),
  search_space = param_mtry, 
  terminator = trm("evals", n_evals = 40),
  tuner = tnr("grid_search"),
  store_models = TRUE)



#train so we can access the best parametersettings
future::plan("multisession")
tnr_ranger$train(task)


save(tnr_ranger, file = paste0("benchmarks/tnr_ranger", as.Date(Sys.time()), ".RData"))

ranger_res <- tnr_ranger$tuning_instance$archive$data()
ranger_res[order(-ranger_res$AUC),c("mtry", "AUC")]
tnr_ranger$tuning_result
tnr_ranger$tuning_instance




# test
lrn_ranger_tuned$train(task)


