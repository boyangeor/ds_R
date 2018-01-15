pr <- function(actual, predicted) {

  # Computes precision and recall from actual realizations and
  # predicted.
  #
  # Args:
  #   actual   : factor, y from the test set
  #   predicted: factor, predicted y from the test set
  #
  # Returns:
  #   Precision and recall for each label.

  cm <- table(actual, predicted)
  if (nrow(cm) != ncol(cm))
    stop("Confusion matrix is not symmetric.")
  
  res.colnames <- paste(c("prec", "rec"),
                        rep(colnames(cm), each=2), sep="_")
  res.colnames <- c(res.colnames, "acc")

  pr <- numeric(length(res.colnames))
  
  for (i in seq_len(nrow(cm))) {
    pr[i * 2 - 1] <- cm[i, i] / sum(cm[, i])
    pr[i * 2]     <- cm[i, i] / sum(cm[i, ])
  }

  pr[length(pr)] <- sum(diag(cm)) / sum(cm)
  
  names(pr) = res.colnames
  print(cm)
  pr
}


