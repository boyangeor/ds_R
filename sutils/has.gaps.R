has.gaps <- function(x) {
  
  # Checks if 'x' is a (increasingly) sorted, continuous, sequence of integers
  #
  # Args:
  #   x: numeric vector
  #
  # Returns:
  #   TRUE if it is sorted, continuous, sequence of integers
  
  if (any(as.logical(x %% 1))) {
    cat("The argument does not contain only integers!\n")
    return(FALSE)
  }
    
  if (is.unsorted(x)) {
    cat("The argument is not sorted in increasing order!\n")
    return(FALSE)
  }
  
  if (all(x == seq_along(x))) {
    cat("The argument is sorted, continuous (by 1), sequence of integers\t")
    return(TRUE)
  }
}