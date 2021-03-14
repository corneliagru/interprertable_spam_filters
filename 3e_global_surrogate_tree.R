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
library(tidyverse)

features=spam %>% select(-(type))

response= spam$type

# ranger surrogate
model=Predictor$new(lrn_ranger_tuned, data=features, y = response)
ranger_tree = TreeSurrogate$new(model, maxdepth = 2)
ranger_tree_pred=predict(ranger_tree, spam, type='class')

# ranger tree plots
plot(ranger_tree)+
  theme_minimal()
plot(ranger_tree$tree)+
  theme_minimal()

# ranger surrogate accuracy
ranger_tree_acc=sum(ranger_tree_pred$.class==spam$type)/4601

# ranger surrogate false positive rate
fp_tree=sum(ranger_tree_pred$.class=='spam' & spam$type=='nonspam')
tn_tree=sum(ranger_tree_pred$.class=='nonspam' & spam$type=='nonspam')
ranger_tree_fpr=fp_tree/(fp_tree+tn_tree)

# svm surrogate
svm_model = Predictor$new(lrn_svm_tuned, data=features, y = response)
svm_tree = TreeSurrogate$new(svm_model, maxdepth = 2)
svm_tree_pred=predict(svm_tree, spam, type='class')

# svm tree plots
plot(svm_tree)+
  theme_minimal()
plot(svm_tree$tree)+
  theme_minimal()

# svm surrogate accuracy
svm_tree_acc=sum(svm_tree_pred$.class==spam$type)/4601

# svm surrogate false positive rate
fp_svm_tree=sum(svm_tree_pred$.class=='spam' & spam$type=='nonspam')
tn_svm_tree=sum(svm_tree_pred$.class=='nonspam' & spam$type=='nonspam')
svm_tree_fpr=fp_svm_tree/(fp_svm_tree+tn_svm_tree)
