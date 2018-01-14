source("~/ds_R/loader.R")

X <- iris
fo <- gen.form(y="Species", data=X)


library(e1071)
library(caret)

res <- e1071::naiveBayes(x=X[1:89, all.vars(fo[[3]])],
                         y=X[1:89, all.vars(fo[[2]])], lambda=1)

# 5-fold cross validation
folds <- caret::createFolds(X[, all.vars(fo[[2]])], k=5)
