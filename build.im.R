build.im <- function(indexcol, data) {
  
  # Args: indexcol - name of variable which has to be indexed
  #       data     - the sorted dataset
  #
  # Returns: a matrix with 2 columns (start, end) rows in the dataset
  
  if (is.unsorted(data[, indexcol]))
    stop("The dataset is not sorted by ", indexcol)
  if (anyNA(data[, indexcol]))
    stop("There are missing values in ", indexcol)
  
  ta <- table(data[, indexcol])
  m <- matrix(0, length(ta), 2, 
              dimnames = list(names(ta), c("start", "end")))
  m[, "end"] <- cumsum(ta)
  m[, "start"] <- c(1, m[, "end"][-nrow(m)] + 1)
  
  m
}