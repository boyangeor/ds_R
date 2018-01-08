att.check <- function(x) {
  
  # Expects data.frame, returns summary of the attributes
  if (!is.data.frame(x))
    stop("The argument's class is not 'data.frame'")
  
  # 01. Numeric attributes.
  num.att <- sapply(x, FUN=is.numeric)
  rn <- paste0(seq_len(sum(num.att)), ". ", names(x)[num.att])
  numeric.m <- matrix(NA, sum(num.att), 4, 
               dimnames=list(rn, c("min", "max", "mean", "sd")))
  
  numeric.m[, "min"] <- sapply(x[num.att], FUN=min)
  numeric.m[, "max"] <- sapply(x[num.att], FUN=max)
  numeric.m[, "mean"] <- sapply(x[num.att], FUN=mean)
  numeric.m[, "sd"] <- sapply(x[num.att], FUN=sd)
  
  # 02. Nominal attributes
  nom.att <- sapply(x, FUN=function(y)
                             (is.factor(y) || is.character(y)))
  
  rn <- paste0(seq_len(sum(nom.att)), ". ", names(x)[nom.att])
  cn <- c("nunique", "minclass", "maxclass","entropy", "max.entropy")
  if (any(nom.att))
    nominal.m <- matrix(NA, sum(nom.att), 5, dimnames=list(rn, cn))
  else
    nominal.m <- NA
  
  for (i in seq_len(sum(nom.att))) {
    nominal.m[i, ] <- sapply(x[nom.att], FUN=function(y) {
      ct <- as.vector(table(y))
      c(length(ct), ct[which.min(ct)], ct[which.max(ct)],
        entropy(ct), max.entropy(ct))
    })
  }
  
  list(numerics=round(numeric.m, 2),
       nominals=round(nominal.m, 2))
}






