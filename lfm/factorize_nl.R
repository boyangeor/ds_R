factorize <- function(dm, r, lambda, eps, epochs, seed) {
  # Factorize dm in rank r matrices L and R.
  
  # dm -  sparse matrix "dgTMatrix" or "matrix" [i, j, value]
  #        
  
  if (inherits(dm, "dgTMatrix")) {
    ijv <- as.matrix(summary(dm))
    m <- nrow(dm)
    n <- ncol(D)
  } else if (inherits(dm, "matrix")) {
    ijv <- dm
    m <- max(dm[, 1])
    n <- max(dm[, 2])
  }
  else
    stop("'dm' should be a 'matrix' or 'dgTMatrix'")
  
  Nis <- table(ijv[, 1])
  Njs <- table(ijv[, 2])
  
  set.seed(seed, kind="Mersenne-Twister")
  L <- matrix(rnorm(m*r), m, r)/sqrt(r)
  R <- matrix(rnorm(r*n), r, n)/sqrt(r)
  
  # Current loss
  cl <- sum((ijv[, 3] - (L %*% R)[cbind(ijv[, 1], ijv[, 2])]) ^ 2) +
        ((0.5 * lambda) * (sum(L ^ 2) + sum(R ^ 2)))
  
  for (ep in seq_len(epochs)) {
    dL <- matrix(0, m, r)
    dR <- matrix(0, r, n)
    
    for (p in seq_len(nrow(ijv))) {
      i <- ijv[p, 1]
      j <- ijv[p, 2]
      
      deviation <- -2 * (ijv[p, 3] - (L[i, ] %*% R[, j]))
      dL[i, ] <- dL[i, ] + deviation * R[, j] +
                 (L[i, ] * (lambda / Nis[i]))
      dR[, j] <- dR[, j] + deviation * L[i, ] +
                 (R[, j] * (lambda / Njs[j]))
    }
    L <- L - eps*dL
    R <- R - eps*dR
    
    ol <- cl
    cl <- sum((ijv[, 3] - (L %*% R)[cbind(ijv[, 1], ijv[, 2])]) ^ 2) +
          ((0.5 * lambda) * (sum(L ^ 2) + sum(R ^ 2)))
    if (ol < cl) { 
      eps <- eps / 2
    } else {
      eps <- eps * 1.05
    }
    
    cat("epoch:", ep, "| loss:", round(cl, 1), "\n")
  }
  
  list(L=L, R=R)
}
