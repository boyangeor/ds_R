library(e1071)
library(rpart)
library(randomForest)

source("~/ds_R/loader.R")

data(iris)
data <- iris
fo <- gen.form(y="Species", data=data)
nfolds <- 5

blah <- explore.models(data, fo, nfolds=5)
t(sapply(blah, colMeans))
sapply(blah, colMeans)


explore.models <- function(data, fo, nfolds) {
  # Runs some models to get first impression
  #
  # Args:
  #   data  : data.set (full no train/test split)
  #   fo    : formula for the model
  #   nfolds: number of folds for quasi cross validation
  #
  # Returns:
  #    Performance metrics for each applied model

  M <- nrow(data)
  N <- ncol(data)
  Ynames <- all.vars(fo[[2]])
  Xnames <- all.vars(fo[[3]])

  # create storage for performance metrics
  pm.cn <- paste(c("P", "R"),
                 rep(levels(data[, Ynames]), each=2), sep="_")
  pm.cn <- c(pm.cn, "acc")
  pmetrics <- rep(list(matrix(NA, nfolds, length(pm.cn),
                              dimnames=list(1:nfolds, pm.cn))), 3)
  names(pmetrics) = c("NB", "DT", "forest")

  # makes relatively small error
  folds <- matrix(sample(M), ceiling(M / nfolds), nfolds)

  for (i in seq_len(nfolds)) {
    train <- as.vector(folds[, -i])
    test  <- folds[, i]

    # Naive Bayes
    res.NB <- e1071::naiveBayes(x = data[train, Xnames],
                                y = data[train, Ynames],
                                lambda = 1)
    pred.NB <- predict(res.nb, data[test, Xnames])
    pmetrics[["NB"]][i, ] <- pr(data[test, Ynames], pred.NB)

    # Decision Tree (CART)
    res.DT <- rpart::rpart(formula=fo, data=data[train, ])
    pred.DT <- predict(res.DT, newdata=data[test, Xnames],
                       type="class")
    pmetrics[["DT"]][i, ] <- pr(data[test, Ynames], pred.DT)

    # Random Forest
    res.forest <- randomForest::randomForest(formula = fo,
                                             data = data,
                                             subset = train)
    pred.forest <- predict(res.forest, data[test, Xnames])
    pmetrics[["forest"]][i, ] <- pr(data[test, Ynames], pred.forest)

    
  }
  lapply(pmetrics, round, 2)
}
