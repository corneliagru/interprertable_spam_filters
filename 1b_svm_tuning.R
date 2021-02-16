#personalized:
#good combination: gamma 0.01, cost 17, AUC 0.97946
#gamma = 0.01, cost = 10, 0.9784371
# list(cost = 13.3333333333333, gamma = 0.01188888888888 AUC 0.9795695


#nonpersonalized:
#list(cost = 10, gamma = 0.01)
#list(cost = 23.3333333333333, gamma = 0.006444444444444) AUC 0.9726138



library(mlr3verse)
library(ggplot2)
set.seed(1234)


# task --------------------------------------------------------------------

# initialize task, if personalized = TRUE all variables are used, 
# otherwise "george", "hp", "num650" are removed
task <- tsk("spam")

personalized <- TRUE

if (!personalized) {
  keep <- setdiff(task$feature_names, c("george", "hp", "num650"))
  task$select(keep)
}

#SVM
# initialize learner
lrn_svm = lrn("classif.svm",
              predict_type = "prob",
              type = "C-classification",
              kernel = "radial")




# large scale -------------------------------------------------------------

# explore large areas of parameters first
gamma <- rep(10^(-8:2), times = 11)
cost <- rep(10^(-2:8), each = 11)
design = data.table::data.table(gamma = gamma, cost = cost)



# initialize search/parameter space
param_svm_l = ParamSet$new(
  params = list(
    ParamDbl$new(id = "cost", lower = 1e-02, upper = 1e+08),
    ParamDbl$new(id = "gamma", lower = 1e-08, upper = 1e+02)
  ))



# initialize auto tuner
tnr_svm_l = AutoTuner$new(
  learner = lrn_svm,
  resampling = rsmp("cv", folds = 5L),
  measure = msr("classif.auc", id = "AUC"),
  search_space = param_svm_l, 
  terminator = trm("evals", n_evals = 150),
  tuner = tnr("design_points", design = design),
  store_models = TRUE)


#for parallelization
# you need future and future.apply installed
# "multicore" for linux and mac
# "multisession" for windows
future::plan("multisession")


# #tuning
# start <- Sys.time()
# tnr_svm_l$train(task)
# end <- Sys.time()
# print(end-start)
# 
# 
# save(tnr_svm_l, file = paste0("benchmarks/tnr_svm", as.Date(Sys.time()),"large_scale", ".RData"))

load(file = "benchmarks/tnr_svm2021-01-24large_scale.RData")



# small scale -------------------------------------------------------------

# tune in promising regions
# initialize search/parameter space
param_svm_s = ParamSet$new(
  params = list(
    ParamDbl$new(id = "cost", lower = 10, upper = 25),
    ParamDbl$new(id = "gamma", lower = 0.001, upper = 0.05)
  ))




tnr_svm_s = AutoTuner$new(
  learner = lrn_svm,
  resampling = rsmp("cv", folds = 5L),
  measure = msr("classif.auc", id = "AUC"),
  search_space = param_svm_s, 
  terminator = trm("evals", n_evals = 150),
  tuner = tnr("grid_search"),
  store_models = TRUE)


#for parallelization
# you need future and future.apply installed
# "multicore" for linux and mac
# "multisession" for windows
future::plan("multisession")


#tuning
start <- Sys.time()
tnr_svm_s$train(task)
end <- Sys.time()
print(end-start)


save(tnr_svm_s, file = paste0("benchmarks/tnr_svm_p_", as.Date(Sys.time()),"small_scale", ".RData"))


# plot results ------------------------------------------------------------


# large scale
plt_l <- tnr_svm_l$tuning_instance$archive$print()

ggplot(plt_l, aes(x = gamma, y = cost, fill = AUC))+
  geom_point()+
  geom_tile()+
  scale_x_log10()+
  scale_y_log10()+
  scale_fill_gradient(low="white", high="#397f78") +
  geom_text(aes(label = round(AUC,digits = 2))) +
  ggtitle("Tuning Result SVM")

ggsave("plots/svm_tuning_large_scale.png", height = 12, width = 18, units = "cm")



#small scale 

plt_s <- tnr_svm_s$tuning_instance$archive$print()

ggplot(plt_s, aes(x = gamma, y = cost, fill = AUC))+
  geom_point()+
  geom_tile()+
  scale_fill_gradient(low="gray", high="blue") +
  geom_text(aes(label = round(AUC,digits = 2)))




