max.entropy.numeric <- function(x) {
  if (!is.vector(x))
    stop("max.entropy requires numeric vector")
  
  log(length(x))
}
