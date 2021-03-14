# SVM ---------------------------------------------------------------------
source('3a_setup_interpretable.R')


pred_svm = Predictor$new(
  svm,
  data = spam[-which(names(spam) == "type")],
  y = (spam$type == "spam"),
  predict.fun = psvm,
  class = "spam"
)



#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)
 

imp_svm <- FeatureImp$new(pred_svm, loss = auc_error)

plot(imp_svm) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)") +
  scale_y_discrete("" ) + #, limits = imp_svm$results[30:1,"feature"]) +
  ggtitle("SVM personalized") +
  theme_minimal()+
  scale_y_discrete("", limits = imp_svm$results[15:1,"feature"])

ggsave("plots/imp_svm_pers.pdf", height = 9, width = 12, units = "cm")



save(imp_svm, file = "data/results/imp_svm.RData")


imp_svm$results

data_plt_imp_svm_p <- imp_svm$results

data_plt_imp_svm_p$feature <- factor(data_plt_imp_svm_p$feature, levels = data_plt_imp_svm_p$feature[order(data_plt_imp_svm_p$importance)])


ggplot(data_plt_imp_svm_p, aes(y = feature, x = importance)) +
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
  ylab("") +
  #xlim(0, 13) +
  ggtitle("SVM personalized") + scale_y_discrete("", limits = data_plt_imp_svm_p[15:1,"feature"])



ggsave("plots/imp_svm_pers.png", height = 9, width = 12, units = "cm")
ggsave("plots/imp_svm_pers.pdf", height = 9, width = 12, units = "cm")






# check influence of paramsettings ----------------------------------------

# svm 17, 0.01

task <- tsk("spam")
spam <- task$data()
spam <- as.data.frame(spam)



lrn_svm_p_old <- lrn("classif.svm",
                 id = "svm_p_old",
                 predict_type = "prob",
                 type = "C-classification",
                 kernel = "radial",
                 cost = 17,
                 gamma = 0.01)


lrn_svm_p_old$train(task)
svm_param <- lrn_svm_p_old$model




pred_svm_param = Predictor$new(
  svm_param,
  data = spam[-which(names(spam) == "type")],
  y = (spam$type == "spam"),
  predict.fun = psvm,
  class = "spam"
)


#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)

imp_svm_param <- FeatureImp$new(pred_svm_param, loss = auc_error)

p_svm_param <- plot(imp_svm_param) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)")+
  ggtitle("SVM personalized - cost = 17, gamma = 0.01")
p_svm_param





# ice ---------------------------------------------------------------------


test <- FeatureEffect$new(pred_svm, "charExclamation", method = "pdp+ice")
plot(test)
test$results

ice <- FeatureEffect$new(pred_svm, "remove", center.at = min(spam$remove), method = "pdp+ice")
plot(ice)

# char !

grid_pts <- seq(min(spam$charExclamation),
                quantile(spam$charExclamation, 0.95), length.out = 30)

ice_svm_charExclamation <- FeatureEffect$new(pred_svm, 
                                             "charExclamation",
                                             center.at = min(spam$charExclamation),
                                             method = "pdp+ice",
                                             grid.points = grid_pts)

plot(ice_charExclamation) + xlim(min(spam$charExclamation), quantile(spam$charExclamation, 0.95))





# ice sample --------------------------------------------------------------



ale_svm_all <- FeatureEffects$new(pred_svm)

ale_svm_all$plot()

plt_svm_ale <- ale_svm_all$plot(features = c("george", "hp", "meeting", "charExclamation", "remove", "charDollar",
                                  "free", "edu", "project", "num000", "num1999", "your"))
plt_svm_ale


ggsave("plots/ale_svm.png", height = 15, width = 20, units = "cm")
ggsave("plots/ale_svm.pdf", height = 15, width = 20, units = "cm")


c("address", "addresses", "all", "business", "capitalAve", 
  "capitalLong", "capitalTotal", "charDollar", "charExclamation", 
  "charHash", "charRoundbracket", "charSemicolon", "charSquarebracket", 
  "conference", "credit", "cs", "data", "direct", "edu", "email", 
  "font", "free", "george", "hp", "hpl", "internet", "lab", "labs", 
  "mail", "make", "meeting", "money", "num000", "num1999", "num3d", 
  "num415", "num650", "num85", "num857", "order", "original", "our", 
  "over", "parts", "people", "pm", "project", "re", "receive", 
  "remove", "report", "table", "technology", "telnet", "will", 
  "you", "your")




# ale receive -------------------------------------------------------------


#receive
ale_receive <- FeatureEffect$new(pred_svm, "receive", method = "ale", grid.size = 50) 

plot(ale_receive)
ggsave("plots/ale_svm_receive.pdf", height = 9, width = 12, units = "cm")


spam[spam$receive>2, ]
