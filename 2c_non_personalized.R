library(mlr3verse)
library(ggplot2)
set.seed(1234)


# task --------------------------------------------------------------------

task_p <- tsk("spam")
task_p$id <- "personalized"

task_np <- tsk("spam")
task_np$id <- "non-personalized"
#remove personalized words
keep <- setdiff(task_np$feature_names, c("george", "hp", "num650"))
task_np$select(keep)

#check if features got removed in np
task_np$feature_names
task_p$feature_names


tasks <- c(task_p, task_np)

# learners ----------------------------------------------------------------

#ranger personalized
lrn_ranger_p = lrn(
  "classif.ranger",
  id = "ranger",
  #id = "ranger_p",
  predict_type = "prob",
  importance = "impurity",
  mtry = 7
)


#ranger non personalized
lrn_ranger_np = lrn(
  "classif.ranger",
  id = "ranger_np",
  predict_type = "prob",
  importance = "impurity",
  mtry = 6
)




#svm personalized
lrn_svm_p <- lrn(
  "classif.svm",
  id = "svm",
  #id = "svm_p",
  predict_type = "prob",
  type = "C-classification",
  kernel = "radial",
  cost = 13.33,
  gamma = 0.012
)

# lrn_svm_p_old <- lrn("classif.svm",
#                  id = "svm_p_old",
#                  predict_type = "prob",
#                  type = "C-classification",
#                  kernel = "radial",
#                  cost = 17,
#                  gamma = 0.01)


# svm non personalized
lrn_svm_np <- lrn(
  "classif.svm",
  id = "svm_np",
  predict_type = "prob",
  type = "C-classification",
  kernel = "radial",
  cost = 23.33,
  gamma = 0.006
)


#learners <- c(lrn_ranger_p, lrn_ranger_np, lrn_svm_p, lrn_svm_np#, lrn_svm_p_old
#              )

learners <- c(lrn_svm_p, lrn_ranger_p)

# benchmarking ------------------------------------------------------------



bm_grid <- benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = rsmp("cv", folds = 10L))


future::plan("multisession")
bm <- benchmark(bm_grid, store_models = TRUE)
print(bm)



autoplot(bm, measure = msr("classif.auc")) + 
  theme_minimal() +
  #scale_x_discrete(labels = model_names)+
  ylab("AUC")



measures = list(
  msr("classif.auc",
      id = "AUC"),
  msr("classif.fpr",
      id = "False Positive Rate"),
  msr("classif.sensitivity",
      id = "Sensitivity"),
  msr("classif.specificity",
      id = "Specificity"),
  msr("classif.ce", 
      id = "MMCE")
)

bm_results <- bm$aggregate(measures)


#for ranger mtry = 7 is better for both

# for svm you can increase AUC a little by using task specific parameters



#plotting
tab = fortify(bm, measure = msr("classif.auc"))
tab$nr = as.character(tab$nr)

# try own plot
ggplot(tab, mapping = aes(x = .data$task_id, y = .data[["classif.auc"]], fill = .data$task_id)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#397f78", "#7ac0b9"))+
  labs(x = "") +
  #scale_x_discrete(labels = rev(unique(tab$task_id))) +
  facet_wrap(vars(.data$learner_id), scales = "free_x") +
  theme(legend.position = "none") +
  ylab("AUC") +
  ggtitle("Performance when removing 'geroge', 'hp' and 'num650'")


ggsave("plots/model_comparison_personalized.png", height = 12, width = 18, units = "cm")

