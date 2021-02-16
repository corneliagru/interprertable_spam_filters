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

# ALE ---------------------------------------------------------------------

#remove
ale_remove <- FeatureEffect$new(predictor, "remove", method = "ale", grid.size = 100) 
ale_remove$results
plot(ale_remove)
imp_res <- imp_res[order(-imp_res$importance),]

ggplot(imp_res, aes(x = importance, y = feature))+
  geom_col(fill = "gray50")+
  scale_y_discrete("",limits = imp_res[20:1, "feature"]) +
  xlab("Feature importance (loss: 1 - AUC)")+
  ggtitle("Permutation FI for Random Forest")

ggsave("plots/imp_ranger_bar.png", height = 9, width = 12, units = "cm")



# ALE ---------------------------------------------------------------------

#TODO does ale with grid points work?
#pdp.remove$center(min(X$remove))
p <- plot(ale_remove) #+ xlim(min(spam$remove), quantile(spam$remove, 0.95))
p
p +
  geom_vline(xintercept = 0.3, col = "red") +
  geom_vline(xintercept = 0.02, col = "red")

#hp

grid_pts <- data.table(seq(min(spam$hp), quantile(spam$hp, 0.95), length.out = 10))
ale_hp <- FeatureEffect$new(predictor, "hp", method = "ale", grid.size = 50) 

plot(ale_hp) +
  scale_x_continuous(limits = c(0,3.6))+
  ggtitle("ALE for hp")

ggsave("plots/ale_hp.png", height = 9, width = 12, units = "cm")


pdp_hp <- FeatureEffect$new(predictor, "hp", method = "pdp", grid.size = 30) 

plot(pdp_hp)+
  scale_x_continuous(limits = c(0,3.6))+
  ggtitle("PDP for hp")

ggsave("plots/pdp_hp.png", height = 9, width = 12, units = "cm")



#num 415

ale_num415 <- FeatureEffect$new(predictor, "num415", method = "ale", grid.size = 50) 

plot(ale_num415)

ggsave("plots/ale_num415.png", height = 9, width = 12, units = "cm")


pdp_num415 <- FeatureEffect$new(predictor, "num415", method = "pdp", grid.size = 30) 

plot(pdp_num415)+
  ggtitle("PDP for num415")

ggsave("plots/pdp_num415.png", height = 9, width = 12, units = "cm")


# find rules

ale_free <- FeatureEffect$new(predictor, "free", method = "ale", grid.size = 50) 

plot(ale_free) +
   geom_vline(xintercept = 2)

ale_free <- FeatureEffect$new(predictor, "free", method = "ale", grid.size = 50) 

ale_capitalAve <- FeatureEffect$new(predictor, "capitalAve", method = "ale", grid.size = 50) 
plot(ale_capitalAve)

ale_capitalAve$results

ale_capitalLong <- FeatureEffect$new(predictor, "capitalLong", method = "ale", grid.size = 50) 
plot(ale_capitalLong)
ale_capitalLong$results

ale_capitalTotal <- FeatureEffect$new(predictor, "capitalTotal", method = "ale", grid.size = 50) 
plot(ale_capitalTotal)
ale_capitalTotal$results


ale_charExclamation <- FeatureEffect$new(predictor, "charExclamation", method = "ale", grid.size = 50) 
plot(ale_charExclamation)
ale_charExclamation$results

#for all 
effs <- FeatureEffects$new(predictor)
plot(effs)
plt_ale <- effs$plot(features = c("charExclamation", "remove", "capitalAve", "free", "hp", "charDollar", 
                         "capitalLong", "you", "your", "our", "george", "capitalTotal"))

plt_ale +
  ggtitle("ALE")

ggsave("plots/ale.png", height = 15, width = 20, units = "cm")


plt_ale <- effs$plot(features = c("charExclamation", "remove", "capitalAve", "free", "hp", "charDollar", 
                       "capitalLong", "you", "your", "our", "george", "capitalTotal"))
plt_ale

ggsave("plots/ale.png", height = 15, width = 20, units = "cm")



eff_plot <- effs$plot(features = top15)
eff_plot


eff_res <- effs$results

eff_res <- dplyr::bind_rows(eff_res)
eff_res$.feature <- factor(eff_res$.feature)


ggplot(eff_res[eff_res$.feature %in% top15,], aes(x = .borders, y = .value)) +
   geom_line()+
  facet_wrap(~.feature, scales = "free")




ggplot(eff_res[eff_res$.feature %in% "remove",], aes(x = .borders, y = .value)) +
  geom_line()+
  geom_vline(xintercept = 0.1, col = "red")


# ice ---------------------------------------------------------------------

ice <- FeatureEffect$new(predictor, "remove", center.at = min(spam$remove), method = "pdp+ice")


plot(ice) + 
  scale_color_discrete(guide='none') + 
  scale_y_continuous('Predicted spam probability')
ice_res <- ice$results

ggplot(ice_res[ice_res$.type=="pdp",], aes(x = remove, y = .value))+
  geom_line()+
  geom_vline(xintercept = 0.3826)



# ice for all -------------------------------------------------------------

top15 <- c("charExclamation", "remove", "capitalAve", "free", "hp", "charDollar", 
  "capitalLong", "you", "your", "our", "george", "capitalTotal", 
  "edu", "charRoundbracket", "re")
for (var in top15) {
  assign(paste0("ice_", var), FeatureEffect$new(predictor, var, center.at = min(spam[,var]), method = "pdp+ice", grid.size = 50))
}

plot(ice_capitalAve)
plot(ice_capitalLong)
plot(ice_capitalTotal)
plot(ice_charDollar)
plot(ice_charExclamation)
plot(ice_charRoundbracket)
plot(ice_edu)
plot(ice_free)
plot(ice_george)
plot(ice_hp)
plot(ice_our)
plot(ice_re)
plot(ice_remove)
plot(ice_you)
plot(ice_your)


summary(spam$free)




