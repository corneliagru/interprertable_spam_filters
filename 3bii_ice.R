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





# free --------------------------------------------------------------------




grid_pts <- grid_pts <- seq(min(spam_smpl$free),
                            quantile(spam_smpl$free, 0.95), length.out = 30)

ice_free_smpl <- FeatureEffect$new(pred_smpl, "free", center.at = min(spam_smpl$free), method = "pdp+ice", grid.points = grid_pts)


ice_free_old <- FeatureEffect$new(predictor, "free", center.at = min(spam_smpl$free), method = "pdp+ice", grid.size = 50)



plot(ice_free_smpl) + 
  ggtitle("PDP + ICE for free") +
  xlim(min(spam_smpl$free), quantile(spam_smpl$free, 0.95))

ggsave("plots/pdp_ice_free.png", height = 9, width = 12, units = "cm")


plot(ice_free_old)




# top15 -------------------------------------------------------------------




top15 <- c("charExclamation", "remove", "capitalAve", "free", "hp", "charDollar", 
           "capitalLong", "you", "your", "our", "george", "capitalTotal", 
           "edu", "charRoundbracket", "re")

for (var in top15[1:2]) {
  grid_pts <- seq(min(spam_smpl[,var]),
                  quantile(spam_smpl[,var], 0.95), length.out = 40)
  assign(paste0("ice_", var, "_smpl"), FeatureEffect$new(pred_smpl, var, 
                                                         center.at = min(spam_smpl[,var]), method = "pdp+ice", grid.points = grid_pts))
  
  
  #TODO create and save plots within loop
  }
for (var in top15) {
  grid_pts <- seq(min(spam_smpl[,var]),
                  quantile(spam_smpl[,var], 0.95), length.out = 40)
  
  assign(paste0("ice_", var, "_smpl"), FeatureEffect$new(pred_smpl, var, center.at = min(spam_smpl[,var]), method = "pdp+ice", grid.points = grid_pts))
}


max(spam_smpl$capitalAve)
grid_pts <- data.table(c(seq(min(spam_smpl$capitalAve),
                             quantile(spam_smpl$capitalAve, 0.9), length.out = 30 ),
                         seq(quantile(spam_smpl$capitalAve, 0.9),
                             max(spam_smpl$capitalAve), length.out = 5 )))
ice_capitalAve_smpl <- FeatureEffect$new(pred_smpl, "capitalAve", center.at = min(spam_smpl$capitalAve), method = "pdp+ice", grid.points = grid_pts)


plot(ice_capitalAve_smpl) + xlim(min(spam_smpl$capitalAve), quantile(spam_smpl$capitalAve, 0.95))
plot(ice_capitalLong_smpl) + xlim(min(spam_smpl$capitalLong), quantile(spam_smpl$capitalLong, 0.95))
plot(ice_capitalTotal_smpl)+ xlim(min(spam_smpl$capitalTotal), quantile(spam_smpl$capitalTotal, 0.95))
plot(ice_charDollar_smpl)+ xlim(min(spam_smpl$charDollar), quantile(spam_smpl$charDollar, 0.95))
plot(ice_charExclamation_smpl)+ xlim(min(spam_smpl$charExclamation), quantile(spam_smpl$charExclamation, 0.95))
plot(ice_charRoundbracket_smpl)+ xlim(min(spam_smpl$charRoundbracket), quantile(spam_smpl$charRoundbracket, 0.95))
plot(ice_edu_smpl)+ xlim(min(spam_smpl$edu), quantile(spam_smpl$edu, 0.95))
plot(ice_free_smpl)+ xlim(min(spam_smpl$free), quantile(spam_smpl$free, 0.95))
plot(ice_george_smpl)+ xlim(min(spam_smpl$george), quantile(spam_smpl$george, 0.95))
plot(ice_hp_smpl) +
  #ggtitle("PDP + ICE for hp") +
  xlim(min(spam_smpl$hp),
       quantile(spam_smpl$hp, 0.95))

#ggsave("plots/pdp_ice_hp.png", height = 9, width = 12, units = "cm")

plot(ice_our_smpl)+ xlim(min(spam_smpl$our), quantile(spam_smpl$our, 0.95))
plot(ice_re_smpl)+ xlim(min(spam_smpl$re), quantile(spam_smpl$re, 0.95))
plot(ice_remove_smpl)+ xlim(min(spam_smpl$remove), quantile(spam_smpl$remove, 0.95))
plot(ice_you_smpl)+ xlim(min(spam_smpl$you), quantile(spam_smpl$you, 0.95))
plot(ice_your_smpl)+ xlim(min(spam_smpl$your), quantile(spam_smpl$your, 0.95))





# remove ------------------------------------------------------------------

grid_pts <- seq(min(spam_smpl$remove),
    quantile(spam_smpl$remove, 0.95), length.out = 30)

ice_remove_smpl <- FeatureEffect$new(pred_smpl, "remove", center.at = min(spam_smpl$remove), method = "pdp+ice", grid.points = grid_pts)


plot(ice_remove_smpl) +xlim(min(spam_smpl$remove), quantile(spam_smpl$remove, 0.95))



plot(ecdf(spam$remove))
abline(h = 0.9)
abline(h = 0.97)

grid_pts <- seq(min(spam$remove),
                quantile(spam$remove, 0.95), length.out = 30)

ice_remove <- FeatureEffect$new(predictor, "remove", center.at = min(spam$remove), method = "pdp+ice", grid.points = grid_pts)

plot(ice_remove) + xlim(min(spam$remove), quantile(spam$remove, 0.95))






# pdp hp

pdp_hp <- FeatureEffect$new(predictor, "hp",  method = "pdp", grid.size = 30)

plot(pdp_hp) +
  ggtitle("PDP for 'hp'")
plot(ice_capitalAve_smpl) + xlim(1,5)
  #xlim(0, quantile(spam$capitalAve, 0.9))
plot(ice_capitalLong_smpl) 
plot(ice_capitalTotal_smpl)
plot(ice_charDollar_smpl)
plot(ice_charExclamation_smpl)
plot(ice_charRoundbracket_smpl)
plot(ice_edu_smpl)
plot(ice_free_smpl)
plot(ice_george_smpl)


  
plot(ice_our_smpl)
plot(ice_re_smpl)
plot(ice_remove_smpl)
plot(ice_you_smpl)
plot(ice_your_smpl)

ggsave("plots/pdp_hp.png", height = 9, width = 12, units = "cm")

# hp sample ---------------------------------------------------------------


grid_pts <- seq(min(spam_smpl$hp),
                quantile(spam_smpl$hp, 0.95), length.out = 30)

ice_hp_smpl <- FeatureEffect$new(pred_smpl, "hp", center.at = min(spam_smpl$hp), method = "pdp+ice", grid.points = grid_pts)

plot(ice_hp_smpl) +
  xlim(min(spam_smpl$hp), quantile(spam_smpl$hp, 0.95)) +
  ggtitle("PDP + ICE for hp - ranger")


ggsave("plots/pdp_ice_hp_ranger.png", height = 9, width = 12, units = "cm")


# remove sample ------------------------------------------------------------------

grid_pts <- seq(min(spam_smpl$remove),
    quantile(spam_smpl$remove, 0.95), length.out = 30)

ice_remove_smpl <- FeatureEffect$new(pred_smpl, "remove", center.at = min(spam_smpl$remove), method = "pdp+ice", grid.points = grid_pts)


plot(ice_remove_smpl) +xlim(min(spam_smpl$remove), quantile(spam_smpl$remove, 0.95))




# remove all --------------------------------------------------------------



plot(ecdf(spam$remove))
abline(h = 0.9)
abline(h = 0.97)

grid_pts <- seq(min(spam$remove),
                quantile(spam$remove, 0.95), length.out = 30)

ice_remove <- FeatureEffect$new(predictor, "remove", center.at = min(spam$remove), method = "pdp+ice", grid.points = grid_pts)

plot(ice_remove) + xlim(min(spam$remove), quantile(spam$remove, 0.95))



# free --------------------------------------------------------------------




grid_pts <- grid_pts <- seq(min(spam_smpl$free),
                            quantile(spam_smpl$free, 0.95), length.out = 30)

ice_free_smpl <- FeatureEffect$new(pred_smpl, "free", center.at = min(spam_smpl$free), method = "pdp+ice", grid.points = grid_pts)

plot(ice_free_smpl) + xlim(min(spam_smpl$free),
                           quantile(spam_smpl$free, 0.95))






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

# SVM ---------------------------------------------------------------------




pred_svm_smpl <- Predictor$new(svm,
                           data = spam_smpl[-which(names(spam_smpl) == "type")],
                           y = (spam_smpl$type == "spam"),
                           predict.fun = psvm,
                           class = "spam")


#free





grid_pts <- seq(min(spam_smpl$free),
                            quantile(spam_smpl$free, 0.95), length.out = 30)



ice_svm_free_smpl <- FeatureEffect$new(pred_svm_smpl, "free", center.at = min(spam_smpl$free), method = "pdp+ice", grid.points = grid_pts)

plot(ice_svm_free_smpl) + xlim(min(spam_smpl$free), quantile(spam_smpl$free, 0.95))




# hp 
grid_pts <- seq(min(spam_smpl$hp),
                quantile(spam_smpl$hp, 0.95), length.out = 30)

ice_svm_hp_smpl <- FeatureEffect$new(pred_svm_smpl, "hp", center.at = min(spam_smpl$hp), method = "pdp+ice", grid.points = grid_pts)

plot(ice_svm_hp_smpl) +
  xlim(min(spam_smpl$hp), quantile(spam_smpl$hp, 0.95)) +
  ggtitle("PDP + ICE for hp - SVM")


ggsave("plots/pdp_ice_hp_svm.png", height = 9, width = 12, units = "cm")


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

plot(ice_charExclamation) + 
  xlim(min(spam_smpl$charExclamation), quantile(spam_smpl$charExclamation, 0.95))+
  ggtitle("PDP + ICE for charExclamation - SVM")

ggsave("plots/pdp_ice_charExclamation_svm.png", height = 9, width = 12, units = "cm")


ice_svm_charExclamation_all <- FeatureEffect$new(pred_svm, "charExclamation", center.at = min(spam$charExclamation), method = "pdp+ice", grid.size = 50)

plot(ice_svm_charExclamation_all) +
  xlim(min(spam$charExclamation),
       2)

ice_svm_charExclamation_all$results





# receive scm -------------------------------------------------------------


ice_svm_receive_all <- FeatureEffect$new(pred_svm, "receive", center.at = min(spam$receive), method = "pdp+ice", grid.size = 50)

plot(ice_svm_receive_all) 




pdp_email_recive = FeatureEffect$new(pred_svm, c("email", "receive"), method = "pdp") 
pdp_email_recive$plot() +
  viridis::scale_fill_viridis(option = "D")+
  ggtitle("PDP interaction of 'email' and 'receive'")


ggsave("plots/pdp_svm_email_receive.png", height = 12, width = 16, units = "cm")



test <- spam[spam$receive>2,]







# exclam free svm ---------------------------------------------------------



pdp_free_exclam = FeatureEffect$new(pred_svm, c("free", "charExclamation"), method = "pdp") 
 
pdp_free_exclam$plot() +
  viridis::scale_fill_viridis(option = "D")+
  ggtitle("PDP interaction of 'free' and 'charExclamation'")


ggsave("plots/pdp_svm_free_charExclamation.png",  height = 12, width = 16, units = "cm")

