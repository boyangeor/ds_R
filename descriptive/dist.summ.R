dist.summ <- function(x) {
  # Args:
  #   x: vector with values (makes sense for revenue)
  #      "Percentage of products that generate percentage of revenue"
  #
  # Returns:
  #   a 20-row matrix
  
  cs <- cumsum(x[order(x, decreasing = T)]) / sum(x)
  
  cu <- cut(cs, (0:20) / 20, labels=FALSE) * (1/20)
  
  tacs <- cumsum(table(cu))
  
  m <- matrix(tacs, 20, 2,
              dimnames=list(names(tacs), c("top.nitems",
                                            "top.items.pct")))
  m[, "top.items.pct"] <- tacs / length(x)
  
  m
}
