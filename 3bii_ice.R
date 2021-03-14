source('3a_setup_interpretable.R')




# preparation -------------------------------------------------------------


numcores <- availableCores()
plan("callr", workers = numcores)

set.seed(1234)
smpl <- sample(seq_len(nrow(spam)), size = 920, replace = FALSE)
spam_smpl <- spam[smpl,]

pred_smpl <- Predictor$new(rng,
                            data = spam_smpl[-which(names(spam_smpl) == "type")],
                            y = (spam_smpl$type == "spam"),
                            predict.fun = pfun,
                            class = "spam")







#  PDP + ALE hp ------------------------------------------------------------

#hp
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



# ALE all features --------------------------------------------------------





#for all 
effs <- FeatureEffects$new(predictor)
plot(effs)
plt_ale <- effs$plot(features = c("charExclamation", "remove", "capitalAve", "free", "hp", "charDollar", 
                                  "capitalLong", "you", "your", "our", "george", "capitalTotal"))

plt_ale 

ggsave("plots/ale_ranger.png", height = 15, width = 20, units = "cm")
ggsave("plots/ale_ranger.pdf", height = 15, width = 20, units = "cm")






# pdp hp ------------------------------------------------------------------

pdp_hp <- FeatureEffect$new(predictor, "hp",  method = "pdp", grid.size = 30)

plot(pdp_hp) +
  ggtitle("PDP for 'hp'")


ggsave("plots/pdp_hp.png", height = 9, width = 12, units = "cm")
ggsave("plots/pdp_hp.pdf", height = 9, width = 12, units = "cm")

# hp sample ---------------------------------------------------------------


grid_pts <- seq(min(spam_smpl$hp),
                quantile(spam_smpl$hp, 0.95), length.out = 30)

ice_hp_smpl <- FeatureEffect$new(pred_smpl, "hp", center.at = min(spam_smpl$hp), method = "pdp+ice", grid.points = grid_pts)

plot(ice_hp_smpl) +
  xlim(min(spam_smpl$hp), quantile(spam_smpl$hp, 0.95)) +
  ggtitle("PDP + ICE for hp - ranger")


ggsave("plots/pdp_ice_hp_ranger.png", height = 9, width = 12, units = "cm")
ggsave("plots/pdp_ice_hp_ranger.pdf", height = 9, width = 12, units = "cm")




#! charExclam
grid_pts <- seq(min(spam$charExclamation),
                quantile(spam$charExclamation, 0.95), length.out = 30)

ice_charExclamation <- FeatureEffect$new(predictor, "charExclamation", center.at = min(spam$charExclamation), method = "pdp+ice", grid.points = grid_pts)

plot(ice_charExclamation) + 
  xlim(min(spam$charExclamation), quantile(spam$charExclamation, 0.95))+
ggtitle("PDP + ICE for charExclamation - ranger")


#sampled 

grid_pts <- seq(min(spam_smpl$charExclamation),
                quantile(spam_smpl$charExclamation, 0.95), length.out = 30)

ice_charExclamation_smpl <- FeatureEffect$new(pred_smpl, "charExclamation", center.at = min(spam_smpl$charExclamation), method = "pdp+ice", grid.points = grid_pts)

plot(ice_charExclamation_smpl) +
  xlim(min(spam_smpl$charExclamation), quantile(spam_smpl$charExclamation, 0.95)) +
  ggtitle("PDP + ICE for charExclamation - ranger")


ggsave("plots/pdp_ice_charExclamation_ranger.png", height = 9, width = 12, units = "cm")
ggsave("plots/pdp_ice_charExclamation_ranger.pdf", height = 9, width = 12, units = "cm")

# SVM ---------------------------------------------------------------------




pred_svm_smpl <- Predictor$new(svm,
                           data = spam_smpl[-which(names(spam_smpl) == "type")],
                           y = (spam_smpl$type == "spam"),
                           predict.fun = psvm,
                           class = "spam")




# hp 
grid_pts <- seq(min(spam_smpl$hp),
                quantile(spam_smpl$hp, 0.95), length.out = 30)

ice_svm_hp_smpl <- FeatureEffect$new(pred_svm_smpl, "hp", center.at = min(spam_smpl$hp), method = "pdp+ice", grid.points = grid_pts)

plot(ice_svm_hp_smpl) +
  xlim(min(spam_smpl$hp), quantile(spam_smpl$hp, 0.95)) +
  ggtitle("PDP + ICE for hp - SVM")


ggsave("plots/pdp_ice_hp_svm.png", height = 9, width = 12, units = "cm")
ggsave("plots/pdp_ice_hp_svm.pdf", height = 9, width = 12, units = "cm")


# hp all
#!
grid_pts <- seq(min(spam_smpl$hp),
                quantile(spam_smpl$hp, 0.95), length.out = 30)

ice_svm_hp <- FeatureEffect$new(pred_svm_smpl, "hp", center.at = min(spam_smpl$hp), method = "pdp+ice", grid.points = grid_pts)

plot(ice_svm_hp) + xlim(min(spam_smpl$hp), quantile(spam_smpl$hp, 0.95))



# charExclamation ---------------------------------------------------------



grid_pts <- seq(min(spam_smpl$charExclamation),
                quantile(spam_smpl$charExclamation, 0.95), length.out = 30)

ice_svm_charExclamation <- FeatureEffect$new(pred_svm_smpl, "charExclamation", center.at = min(spam_smpl$charExclamation), method = "pdp+ice", grid.points = grid_pts)

plot(ice_svm_charExclamation) + 
  xlim(min(spam_smpl$charExclamation), quantile(spam_smpl$charExclamation, 0.95))+
  ggtitle("PDP + ICE for charExclamation - SVM")

ggsave("plots/pdp_ice_charExclamation_svm.png", height = 9, width = 12, units = "cm")
ggsave("plots/pdp_ice_charExclamation_svm.pdf", height = 9, width = 12, units = "cm")


ice_svm_charExclamation_all <- FeatureEffect$new(pred_svm, "charExclamation", center.at = min(spam$charExclamation), method = "pdp+ice", grid.size = 50)

plot(ice_svm_charExclamation_all) +
  xlim(min(spam$charExclamation),
       2)

ice_svm_charExclamation_all$results





# receive svm -------------------------------------------------------------


ice_svm_receive_all <- FeatureEffect$new(pred_svm, "receive", center.at = min(spam$receive), method = "pdp+ice", grid.size = 50)

plot(ice_svm_receive_all) 




pdp_email_recive = FeatureEffect$new(pred_svm, c("email", "receive"), method = "pdp") 
pdp_email_recive$plot() +
  viridis::scale_fill_viridis(option = "D")+
  ggtitle("PDP interaction of 'email' and 'receive'")


ggsave("plots/pdp_svm_email_receive.png", height = 12, width = 16, units = "cm")






# exclam free svm ---------------------------------------------------------



pdp_free_exclam = FeatureEffect$new(pred_svm, c("free", "charExclamation"), method = "pdp") 
 
pdp_free_exclam$plot() +
  viridis::scale_fill_viridis(option = "D")+
  ggtitle("PDP interaction of 'free' and 'charExclamation'")


ggsave("plots/pdp_svm_free_charExclamation.png",  height = 12, width = 16, units = "cm")

