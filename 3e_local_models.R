source('3a_setup_interpretable.R')

library(devtools)
library(DALEX)
library(DALEXtra)
library(iml)
library(partykit)
library(trtf)
library(mosmafs)
library(GGally)
library(ggExtra)
library(metR)

devtools::load_all("../moc/iml", export_all = FALSE)
devtools::load_all("../moc/counterfactuals", export_all = FALSE)

prediction <- lrn_ranger_tuned$predict(task)

df_pred=as.data.table(prediction)
false=df_pred[prediction$truth!=prediction$response]
worst=false[(false$prob.spam<0.2 & false$truth=='spam') | (false$prob.nonspam<0.2 & false$truth=='nonspam')]
close=false[abs(false$prob.nonspam-false$prob.spam)<0.1]

# plotting worst predictions
pdf(file="./plots/worst_predictions.pdf")
ggplot(worst, aes(y=prob.spam, x=row_id, color=truth))+
  geom_point(size=3)+
  geom_rug()+
  labs(title='Worst Predictions', y='Prediction Probability of Spam', x='Instance ID')
dev.off()

# plotting close predictions
pdf(file="./plots/close_predictions.pdf")
ggplot(close, aes(y=prob.spam, x=row_id, color=truth))+
  geom_point(size=3)+
  geom_rug()+
  geom_abline(aes(slope=0, intercept=0.5))+
  labs(title='Close Predictions', y='Prediction Probability of Spam', x='Instance ID')
dev.off()

# taking explainable out from the learner
pfun <- function(model, newdata) {
  predict(model, newdata)$predictions[,1]}

ranger_exp <- explain_mlr3(rng,
                           data     = spam[-which(names(spam) == "type")],
                           y        = as.numeric(spam$type == "spam"),
                           predict_function = pfun,
                           label    = "Tuned Random Forest",
                           colorize = FALSE)

svm_exp <- explain_mlr3(svm,
                           data     = spam[-which(names(spam) == "type")],
                           y        = as.numeric(spam$type == "nonspam"),
                           label    = "Tuned SVM",
                           colorize = FALSE)


# MODEL PERFORMANCE
mp_rf <- model_performance(ranger_exp)
mp_svm <- model_performance(svm_exp)

plot(mp_rf, mp_svm)

plot(mp_rf, mp_svm, geom = 'boxplot')

ggplot(df_pred,
       aes(truth, prob.spam, color=truth)) +
  geom_abline(aes(slope=0, intercept=0.5))+
  geom_rug()+
  geom_point() +
  xlab("Observed") +
  ylab("Prediction Probability") +
  ggtitle("Diagnostic plot")


# GLOBAL MODELS
# getting variable importance
imp_rf <- model_parts(ranger_exp, type='ratio')

# plotting feature importance
plot(imp_rf, max_vars = 12, show_boxplots = FALSE)

# defining selected variables
var <- c("charExclamation", "remove")


# partial dependence profiles
pd_rf <- model_profile(ranger_exp, type = 'partial')

# accumulated-local profiles
al_rf <- model_profile(ranger_exp, type = "accumulated")

# local-dependence profiles
ld_rf <- model_profile(ranger_exp, type = 'conditional')

# clustered ale 
al_rf_clust <- model_profile(ranger_exp, type = "accumulated", k = 2)

# plotting pdp
plot(pd_rf, variables = var, geom='profiles')+
  scale_y_continuous("Classification Probability: Spam(1) or Nonspam(0)")+
  ggtitle("Partial Dependence Plots for Selected Variables")

# plotting pdp with points
plot(pd_rf, variables = var, geom = "points")+
  scale_y_continuous("Classification Probability: Spam(1) or Nonspam(0)")+
  ggtitle("PDP Plots for Selected Variables")

# plotting ale with 2 clusters
plot(al_rf_clust, variables = var, geom = "profiles")

# plotting ale
plot(al_rf, variables = var, geom = "profiles")

# plottin ale, ldp and pdp
pd_rf$agr_profiles$`_label_` = "partial dependence"
ld_rf$agr_profiles$`_label_` = "local dependence"
al_rf$agr_profiles$`_label_` = "accumulated local"

plot(pd_rf, ld_rf, al_rf, variables=var) 


# LOCAL MODELS
worst_rows=worst$row_id
close_rows=close$row_id


# break-down profiles
bd_rf <- predict_parts(explainer = ranger_exp,
                       new_observation = spam[close_rows[13],],
                       type = "break_down")

# break-down profiles for model with interactions
ibd_rf <- predict_parts(explainer = ranger_exp,
                        new_observation = spam[close_rows[13],],
                        type = "break_down_interactions")

# shapley values
shap_rf <- predict_parts(explainer = ranger_exp,
                         new_observation = spam[close_rows[13],],
                         type = "shap")

# plotting break down profile
plot(bd_rf)

# plotting break-down profile with interactions
plot(ibd_rf)

# plotting shapley values
plot(shap_rf)


# MODEL DIAGNOSTICS
md_rf <- model_diagnostics(ranger_exp)

plot(md_rf, variable = "ids", yvariable = "residuals")

plot(md_rf, variable = "y_hat", yvariable = "abs_residuals")

# SURROGATE MODEL
tree = TreeSurrogate$new(predictor, maxdepth = 4)
plot(tree)

# COUNTERFACTUALS
pfun <- function(object, newdata) predict(object, data = newdata)$predictions

predictor = Predictor$new(rng,
                          data = spam[-which(names(spam) == "type")],
                          y = (spam$type == "spam"),
                          predict.fun = pfun,
                          class = "spam")


x.interest =spam[close_rows[13],]

best.params = readRDS("../moc/saved_objects/best_configs.rds")  # generated by irace in folder appendix_irace

ctr = partykit::ctree_control(maxdepth = 5L)

predictor$conditionals = fit_conditionals(predictor$data$get.x(), ctrl = ctr)

predictor$predict(spam[close_rows[13],])

system.time({ranger.cf = Counterfactuals$new(predictor = predictor, 
                                             x.interest = x.interest, 
                                             target = c(0, 0.5), epsilon = 0, generations = list(mosmafs::mosmafsTermStagnationHV(10),
                                                                                                 mosmafs::mosmafsTermGenerations(200)), 
                                             mu = best.params$mu, 
                                             p.mut = best.params$p.mut, p.rec = best.params$p.rec, 
                                             p.mut.gen = best.params$p.mut.gen, 
                                             p.mut.use.orig = best.params$p.mut.use.orig, 
                                             p.rec.gen = best.params$p.rec.gen, initialization = "icecurve",
                                             p.rec.use.orig = best.params$p.rec.use.orig)})


# Number of counterfactuals
nrow(ranger.cf$results$counterfactuals)
id = ranger.cf$results$counterfactuals$dist.target == 0
sum(id)

# Focus counterfactuals that met target
ranger.cf$results$counterfactuals = ranger.cf$results$counterfactuals[which(id), ]
ranger.cf$results$counterfactuals.diff = ranger.cf$results$counterfactuals.diff[which(id), ]

# Get relative frequency of feature changes
ranger.cf$get_frequency()

###---- Plots ----
a = ranger.cf$plot_parallel(features = c("charDollar", "free"), plot.x.interest = FALSE)
a = a + scale_x_discrete(expand = c(0.1, 0.1), labels= c("charDollar", "free"))
a
b = ranger.cf$plot_surface(features = c("charDollar", "free"))
b
c = ranger.cf$plot_hv()
c