library("future")
library("future.callr")

source('3a_setup_non_personalized.R')

# feature importance ------------------------------------------------------


#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)


imp_np <- FeatureImp$new(pred_np, loss = auc_error)

imp_np$results[1:15,]

plot(imp_np) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)", limits = c(0,15)) +
  ggtitle("Ranger non-personalized") +
  theme_minimal() +
  scale_y_discrete("", limits = imp_np$results[15:1,"feature"])


ggsave("plots/imp_ranger_non_pers.png", height = 9, width = 12, units = "cm")

# svm ---------------------------------------------------------------------

pred_svm_np = Predictor$new(svm_np,
                          data = spam_np[-which(names(spam_np) == "type")],
                          y = (spam_np$type == "spam"),
                          predict.fun = psvm,
                          class = "spam")


#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)



imp_svm_np <- FeatureImp$new(pred_svm_np, loss = auc_error)


plot(imp_svm_np) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)") + #, limits = c(0,11)
  ggtitle("SVM non-personalized") +
  theme_minimal() +   
  scale_y_discrete("", limits = imp_svm_np$results[15:1,"feature"]) #

ggsave("plots/imp_svm_non_pers.png", height = 9, width = 12, units = "cm")



data_plt_imp_svm_np <- imp_svm_np$results

data_plt_imp_svm_np$feature <- factor(data_plt_imp_svm_np$feature, levels = data_plt_imp_svm_np$feature[order(data_plt_imp_svm_np$importance)])


ggplot(data_plt_imp_svm_np, aes(y = feature, x = importance)) +
  geom_segment(
    aes(
      y = feature,
      yend = feature,
      x = importance.05,
      xend = importance.95
    ),
    size = 1,
    color = "darkslategrey"
  ) +
  geom_point(size = 1) +
  xlab("Feature importance (loss: 1 - AUC)") +
  scale_y_discrete("", limits = data_plt_imp_svm_np[15:1,"feature"]) +
  ggtitle("SVM non-personalized") +
  theme_minimal()


ggsave("plots/imp_svm_non_pers.png", height = 9, width = 12, units = "cm")



# influence of paramsettings ----------------------------------------------


#non personalized

lrn_svm_old_np <- lrn("classif.svm",
                     id = "svm_p_old",
                     predict_type = "prob",
                     type = "C-classification",
                     kernel = "radial",
                     cost = 17,
                     gamma = 0.01)


lrn_svm_old_np$train(task_np)
svm_param_np <- lrn_svm_p_old_np$model




pred_svm_param_np = Predictor$new(
  svm_param_np,
  data = spam[-which(names(spam) == "type")],
  y = (spam$type == "spam"),
  predict.fun = psvm,
  class = "spam"
)


#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)

imp_svm_param_np <- FeatureImp$new(pred_svm_param_np, loss = auc_error)

p_svm_param_np <- plot(imp_svm_param_np) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)")+
  ggtitle("SVM non-personalized - cost = 17, gamma = 0.01")
p_svm_param_np






# personalized


lrn_svm_old_p <- lrn("classif.svm",
                        id = "svm_p_old",
                        predict_type = "prob",
                        type = "C-classification",
                        kernel = "radial",
                        cost = 17,
                        gamma = 0.01)


lrn_svm_old_p$train(task)
svm_param_p <- lrn_svm_old_p$model




pred_svm_param_p = Predictor$new(
  svm_param_p,
  data = spam[-which(names(spam) == "type")],
  y = (spam$type == "spam"),
  predict.fun = psvm,
  class = "spam"
)


#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)

imp_svm_param_p <- FeatureImp$new(pred_svm_param_p, loss = auc_error)

p_svm_param_p <- plot(imp_svm_param_p) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)")+
  ggtitle("SVM personalized - cost = 17, gamma = 0.01")

p_svm_param_p



#approximately same scale
gridExtra::grid.arrange(p_svm_param_np, p_svm_param_p, nrow  = 1)





# ale ---------------------------------------------------------------------

effs_np <- FeatureEffects$new(pred_np)
plot(effs_np)


plt_ale_np <- effs_np$plot(features = c("charExclamation", "remove", "capitalAve", "free",
                          "charDollar","capitalLong", "you", "your", "our",
                          "capitalTotal"))





plt_ale_np

