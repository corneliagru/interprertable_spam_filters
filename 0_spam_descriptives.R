# Setup
library(mlr3verse)
library(ggplot2)
set.seed(1234)

# task --------------------------------------------------------------------
# initialize task with data
task <- tsk("spam")
spam <- task$data()

write.csv2(spam, "spam.csv")
prop.table(table(spam$type))

colnames(spam)

hist(spam$num415)

for (variable in colnames(spam)[1:10]) {
  print(ggplot(spam, aes_string(variable, fill = "type", group = "type"))+
    geom_density()+ 
    facet_grid(~type))
  
}



for (variable in colnames(spam)[1:10]) {
  print(ggplot(spam, aes_string(x = "type", y=variable))+
          geom_boxplot())
  
}










# descriptives ------------------------------------------------------------


ggplot(spam, aes(remove)) + 
  geom_density()

ggplot(spam, aes(remove, fill = type)) + 
  geom_boxplot()+
  xlim(0,0.5)


#TODO: descriptives


str(spam)





library(corrplot)

res <- cor(spam[,c(
                   "hp",
                   "hpl",
                   "labs",
                   "lab",
                   "technology",
                   "telnet",
                   "direct",
                   "num415",
                   "num857",
                   "num650",
                   "num85")])

corrplot(res, order = "hclust")

corrplot.mixed(res, order = "hclust")
