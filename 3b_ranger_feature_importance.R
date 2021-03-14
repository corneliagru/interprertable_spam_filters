library("future")
library("future.callr")


source('3a_setup_interpretable.R')



#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)



# Feature Importance ------------------------------------------------------




#Ratio: error.permutation/error.orig is default
imp <- FeatureImp$new(predictor, loss = auc_error, compare = "ratio")

plot(imp) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)", limits = c(0,11)) +
  ggtitle("Ranger personalized") +
  theme_minimal() +
  scale_y_discrete("", limits = imp$results[15:1,"feature"])

ggsave("plots/imp_ranger_pers.png", height = 9, width = 12, units = "cm")
ggsave("plots/imp_ranger_pers.pdf", height = 9, width = 12, units = "cm")

imp$results

res <- imp$results


top15 <- as.character(res$feature[1:15])
#res$feature <- factor(res$feature)
top15 <- c("charExclamation", "remove", "capitalAve", "free", "hp", "charDollar", 
           "capitalLong", "you", "your", "our", "george", "capitalTotal", 
           "edu", "charRoundbracket", "re")


ggsave("plots/imp_ranger_pers.png", height = 9, width = 12, units = "cm")

#Interpretation: Permuting feature xxx resulted in an increase in 1-AUC by a
#factor of yyy




imp_res <- imp$results

imp_res <- imp_res[order(-imp_res$importance),]

ggplot(imp_res, aes(x = importance, y = feature))+
  geom_col(fill = "gray50")+
  scale_y_discrete("",limits = imp_res[20:1, "feature"]) +
  xlab("Feature importance (loss: 1 - AUC)")+
  ggtitle("Permutation FI for Random Forest")

ggsave("plots/imp_ranger_bar.png", height = 9, width = 12, units = "cm")
ggsave("plots/imp_ranger_bar.pdf", height = 9, width = 12, units = "cm")


