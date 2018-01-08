entropy.numeric <- function(x) {
  if (!is.vector(x))
    stop("entropy requires numeric vector")
  
  p <- x / sum(x)
  -(sum(p * log(p), na.rm=T))
}
