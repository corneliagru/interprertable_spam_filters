source('~/R/IML/3a_setup_interpretable.R')




test <- predict(object = svm, newdata = spam[-which(names(spam) == "type")] , probability = TRUE)


# Feature Importance ------------------------------------------------------


#check if future works 
bench::system_time({
  plan(sequential)
  FeatureImp$new(predictor, loss = auc_error)
})
# process    real 
# 11.84m   2.84m 
bench::system_time({
  plan("callr", workers = numcores)
  FeatureImp$new(predictor, loss = auc_error)
})
# process    real 
# 1.43m   1.66m 




#can be {'ce','f1','logLoss','mae','mse','rmse','mape','mdae','msle',
# 'percent_bias','rae','rmsle','rse','rrse','smape'} 
# imp <- FeatureImp$new(predictor, loss = "ce")
imp <- FeatureImp$new(predictor, loss = auc_error)
#imp <- FeatureImp$new(predictor, loss = "auc")
p_imp <- plot(imp) +
  scale_x_continuous("Feature importance (loss: 1 - AUC)")
p_imp
imp$results

res <- imp$results

#res$feature = with(res, reorder(feature, importance, median))
res$feature <- factor(res$feature, levels = res$feature[order(res$importance)])

top15 <- as.character(res$feature[1:15])
#res$feature <- factor(res$feature)
ggplot(res[1:15,], aes(y = feature, x = importance)) +
  geom_segment(aes(y = feature, yend = feature,
                   x = importance.05, xend = importance.95),
               size = 1.5, color = "darkslategrey") +
  geom_point(size = 3) +
  xlab("Feature importance (loss: 1 - AUC)") +
  ylab("") +
  ggtitle("Feature Importance")

#Interpretation: Permuting feature xxx resulted in an increase in 1-AUC by a
#factor of yyy





# PDP ---------------------------------------------------------------------

pdp.remove <- FeatureEffect$new(predictor, "remove") 
#pdp.remove$center(min(X$remove))
p <- plot(pdp.remove)
p
p +
  geom_vline(xintercept = 0.3, col = "red") +
  geom_vline(xintercept = 0.02, col = "red")



#for all 
effs <- FeatureEffects$new(predictor)
plot(effs)
effs$plot(features = c("remove", "charExclamation", "hp", "free", "charDollar",
                       "capitalAve"))


eff_plot <- effs$plot(features = top15)


eff_res <- effs$results

eff_res <- dplyr::bind_rows(eff_res)
eff_res$.feature <- factor(eff_res$.feature)


ggplot(eff_res[eff_res$.feature %in% top15,], aes(x = .borders, y = .value)) +
   geom_line() +
  facet_wrap(~.feature, scales = "free")




ggplot(eff_res[eff_res$.feature %in% "remove",], aes(x = .borders, y = .value)) +
  geom_line() +
  geom_vline(xintercept = 0.1, col = "red")


# ice ---------------------------------------------------------------------

ice <- FeatureEffect$new(predictor, "remove", center.at = min(spam$remove), method = "pdp+ice")


plot(ice) + 
  scale_color_discrete(guide = 'none') + 
  scale_y_continuous('Predicted spam probability')
ice_res <- ice$results

ggplot(ice_res[ice_res$.type == "pdp",], aes(x = remove, y = .value)) +
  geom_line() +
  geom_vline(xintercept = 0.3826)




