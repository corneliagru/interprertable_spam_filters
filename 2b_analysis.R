
source('2a_setup_learners.R')


# "classif.xgboost" and ranger tuned
learners <- c("classif.featureless","classif.naive_bayes", "classif.rpart",
              "classif.glmnet",
              "classif.kknn", "classif.log_reg")
learners <- sapply(learners, lrn, predict_type = "prob")

# add our tuned learners
learners <- append(learners, c(svm = lrn_svm_tuned, ranger = lrn_ranger_tuned, xgb = lrn_xgb_tuned))


# applying the learners ---------------------------------------------------

bm_grid <- benchmark_grid(
  tasks = task,
  learners = learners,
  resamplings = rsmp("cv", folds = 5L))



#for parallelization
# you need future and future.apply installed
# "multicore" for linux and mac
# "multisession" for windows
future::plan("multisession")


time <- Sys.time()
bm <- benchmark(bm_grid, store_models = TRUE)
duration <- Sys.time() - time
print(duration)


print(bm)

model_names <- c("featureless","naive_bayes", "rpart",
                 "glmnet", "knn", "log_reg",  "svm",
                  "ranger", "xgboost")

autoplot(bm, measure = msr("classif.auc")) + 
  theme_minimal() +
  scale_x_discrete(labels = model_names) +
  ylab("AUC") +
  ggtitle("Model Comparison")


ggsave("plots/model_comparison.png", height = 12, width = 18, units = "cm")


tab = fortify(bm, measure = msr("classif.auc"))
tab$nr = as.character(tab$nr)
# try own plot
ggplot(tab, mapping = aes(x = .data$nr, y = .data[["classif.auc"]], fill = .data$nr )) +
  geom_boxplot() +
  labs(x = "") +
  scale_x_discrete(labels = model_names) +
  theme(legend.position = "none") +
  ylab("AUC") +
  ggtitle("Model Comparison")

ggsave("plots/model_comparison.png", height = 12, width = 18, units = "cm")


# plot for fpr

autoplot(bm, measure = msr("classif.fpr")) + 
  theme_minimal() +
  scale_x_discrete(labels = model_names) +
  ylab("FPR") +
  ggtitle("Model Comparison")






autoplot(bm, measure = msr("classif.fpr")) + 
  theme_minimal() +
  scale_x_discrete(limits = "classif.svm") + #c("classif.featureless","classif.svm", "classif.ranger", "classif.xgboost")
  ylab("FPR") +
  ggtitle("Model Comparison")







tab = fortify(bm, measure = msr("classif.fpr"))
tab$nr = as.character(tab$nr)
tab <- tab[tab$learner_id %in% c("classif.featureless","classif.svm", "classif.ranger", "classif.xgboost"),]
# try own plot
ggplot(tab, mapping = aes(x = .data$nr, y = .data[["classif.fpr"]], fill = .data$nr )) +
  geom_boxplot() +
  #scale_fill_gradient(low="white", high="#397f78") +
  labs(x = "") +
  scale_x_discrete(labels = model_names) +
  theme(legend.position = "none") +
  ylab("FPR") +
  ggtitle("Model Comparison - FPR")






 measures = list(
  msr("classif.auc",
      id = "AUC"),
  msr("classif.fpr",
      id = "False Positive Rate"),
  # msr("classif.sensitivity",
  #     id = "Sensitivity"),
  # msr("classif.specificity",
  #     id = "Specificity"),
  msr("classif.ce", 
      id = "CE")
)
 
 
 
 
 tab = fortify(bm, measure = msr("classif.fpr"))
 tab$nr = as.character(tab$nr)
 tab <- tab[tab$learner_id %in% c("classif.featureless","classif.svm", "classif.ranger", "classif.xgboost"),]
 # try own plot
 ggplot(tab, mapping = aes(x = .data$nr, y = .data[["classif.fpr"]], fill = .data$nr )) +
   geom_boxplot() +
   scale_fill_manual(values = c("white", "#c9f4f8","#7ac0b9", "#397f78")) +
   scale_x_discrete("", labels = c("featureless",  "svm",
                                   "ranger", "xgboost")) +
   theme(legend.position = "none") +
   ylab("FPR") +
   ggtitle("Model Comparison - FPR")
 
 
 ggsave("plots/model_comparison_fpr.png", height = 9, width = 12, units = "cm")
 
 
 
 
 

bm_results <- bm$aggregate(measures)

bm_table <- bm_results[,c("nr","learner_id" ,"AUC", "False Positive Rate", "CE")]

bm_table[,c("AUC", "False Positive Rate", "CE")] <- round(bm_table[,c("AUC", "False Positive Rate", "CE")], digits = 3)

write.table(bm_table, "data/results/bm_table.txt", sep = ",", quote = FALSE, row.names = F)





# rpart -------------------------------------------------------------------
  
  lrn_rpart <- lrn("classif.rpart", predict_type = "prob")
lrn_rpart$train(task) 
plot(lrn_rpart$model, margin = 0.09, uniform = TRUE)
text(lrn_rpart$model,use.n = TRUE)


# save results ------------------------------------------------------------

# save(bm, tnr_ranger, tnr_xgb, file = paste0(
#  "benchmarks/benchmark-", as.Date(Sys.time()), "-v2", ".RData"))

load("benchmarks/benchmark-2020-11-30-v2.RData")


# save(tnr_ranger, file = paste0(
#   "benchmarks/tnr_ranger", as.Date(Sys.time()), ".RData"))

load("benchmarks/tnr_ranger2020-12-26.RData")



