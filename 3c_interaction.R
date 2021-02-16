source('3a_setup_interpretable.R')


#initialize parallelization
numcores <- availableCores()
plan("callr", workers = numcores)




start <- Sys.time()
ia = Interaction$new(predictor, grid.size = 30) 
end <- Sys.time()
end-start



ia_res <- ia$results
ia_res <- ia_res[order(-ia_res$.interaction),]


ia_features <- ia_res[15:1,".feature"][[1]]

plot(ia) +
  scale_y_discrete("", limits = ia_features) +
  ggtitle("Interaction - Ranger")
ggsave("plots/ia.png", height = 9, width = 12, units = "cm")




save(ia, file = "data/results/ia.RData")
load("data/results/ia.RData")


#check interaction  charExclamation -------------------------------------
numcores <- availableCores()
plan("callr", workers = numcores)

bench::system_time({
  ia_exclam = Interaction$new(predictor, grid.size = 100,
                              feature = "charExclamation") 
})

ia_excl_res <- ia_exclam$results
ia_excl_res <- ia_excl_res[order(-ia_excl_res$.interaction),]

plot(ia_exclam) + scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("", limits = ia_excl_res[15:1,".feature"][[1]])+
  ggtitle("Interactions with 'charExclamation'")
ggsave("plots/ia_exclam.png", height = 9, width = 12, units = "cm")



#save(ia_exclam, file = "data/results/ia_exclam.RData")
load("data/results/ia_exclam.RData")



#check interaction between remove  -------------------------------------
start <- Sys.time()
ia_remove = Interaction$new(predictor, grid.size = 100,
                              feature = "remove") 
end <- Sys.time()
end-start

save(ia_remove, file = "data/results/ia_remove.RData")
load("data/results/ia_remove.RData")



plot(ia_remove) + scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("")

ia_remove$results




# free --------------------------------------------------------------------


# start <- Sys.time()
# ia_free = Interaction$new(predictor, grid.size = 100,
#                             feature = "free") 
# end <- Sys.time()
# end-start
# save(ia_free, file = "data/results/ia_free.RData")
load("data/results/ia_free.RData")






plot(ia_free) + 
  scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("") 



ia_free$results


data_plt_ia_free <- ia_free$results[ia_free$results$.interaction>0.05,]
data_plt_ia_free <- data_plt_ia_free[order(-data_plt_ia_free$.interaction),]



dput(data_plt_ia_free$.feature)


ggplot(data_plt_ia_free, aes(y = .feature, x = .interaction)) +
  geom_point() +
  geom_segment(aes(yend = .feature, x = 0, xend = .interaction)) +
  scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("", limits = rev(c("capitalLong:free", "money:free", "num000:free", "charDollar:free", 
                                          "charExclamation:free", "capitalAve:free", "remove:free", "business:free", 
                                          "you:free", "credit:free", "internet:free", "receive:free", "font:free", 
                                          "george:free", "our:free"))) +
  ggtitle("Interactions with 'free'")



ggsave("plots/ia_free.png", height = 9, width = 12, units = "cm")



# interaction dollar ------------------------------------------------------

start <- Sys.time()
ia_dollar = Interaction$new(predictor, grid.size = 100,
                           feature = "charDollar") 
end <- Sys.time()
end-start
save(ia_dollar, file = "data/results/ia_dollar.RData")
load("data/results/ia_dollar.RData")



plot(ia_dollar)




data_plt_ia_dollar <- ia_dollar$results[ia_dollar$results$.interaction>0.05,]
data_plt_ia_dollar <- data_plt_ia_dollar[order(-data_plt_ia_dollar$.interaction),]



dput(data_plt_ia_dollar$.feature)


ggplot(data_plt_ia_dollar, aes(y = .feature, x = .interaction)) +
  geom_point() +
  geom_segment(aes(yend = .feature, x = 0, xend = .interaction)) +
  scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("Features", limits = rev(c("remove:charDollar", "capitalAve:charDollar", "charExclamation:charDollar", 
                                              "free:charDollar", "money:charDollar", "business:charDollar", 
                                              "capitalTotal:charDollar", "all:charDollar", "our:charDollar", 
                                              "hp:charDollar", "capitalLong:charDollar", "num000:charDollar", 
                                              "meeting:charDollar", "your:charDollar", "email:charDollar"))) +
  ggtitle("Interactions with 'charDollar'")


ggsave("plots/ia_dollar.png", height = 9, width = 12, units = "cm")

# SVM ---------------------------------------------------------------------



future::plan("multisession")




start <- Sys.time()
ia_svm = Interaction$new(pred_svm) 
end <- Sys.time()
end-start

save(ia_svm, file = "data/results/ia_svm.RData")
load("data/results/ia_svm.RData")



ia_svm_res <- ia_svm$results
ia_svm_res <- ia_svm_res[order(-ia_svm_res$.interaction),]
ia_svm_features <- ia_svm_res[15:1,".feature"][[1]]


plot(ia_svm) +
  scale_y_discrete("", limits = ia_svm_features) +
  ggtitle("Interaction - SVM")
ggsave("plots/ia_svm.png", height = 9, width = 12, units = "cm")


start <- Sys.time()
ia_svm_receive = Interaction$new(pred_svm, 
                            feature = "receive") 

end <- Sys.time()
end-start



ia_svm_res_receive <- ia_svm_receive$results
ia_svm_res_receive <- ia_svm_res_receive[order(-ia_svm_res_receive$.interaction),]
ia_svm_features_receive  <- ia_svm_res_receive[15:1,".feature"][[1]]




plot(ia_svm_receive) + 
  scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("", limits = ia_svm_features_receive)+
  ggtitle("Interactions with 'receive' - SVM")
ggsave("plots/ia_svm_receive.png", height = 9, width = 12, units = "cm")


save(ia_svm_receive, file = "data/results/ia_svm_receive.RData")
#load("data/results/ia_svm_receive.RData")


start <- Sys.time()
ia_svm_exclam = Interaction$new(pred_svm, 
                                feature = "charExclamation") 

end <- Sys.time()
end-start

save(ia_svm_exclam, file = "data/results/ia_svm_exclam.RData")
#load("data/results/ia_svm_exclam.RData")


plot(ia_svm_exclam)



ia_svm_res_charExclamation <- ia_svm_exclam$results
ia_svm_res_charExclamation <- ia_svm_res_charExclamation[order(-ia_svm_res_charExclamation$.interaction),]
ia_svm_features_charExclamation  <- ia_svm_res_charExclamation[15:1,".feature"][[1]]




plot(ia_svm_exclam) + 
  scale_x_continuous("2-way interaction strength") +
  scale_y_discrete("", limits = ia_svm_features_charExclamation)+
  ggtitle("Interactions with '!' - SVM")
ggsave("plots/ia_svm_charExclamation.png", height = 9, width = 12, units = "cm")
