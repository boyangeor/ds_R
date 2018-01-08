gen.form <- function(y, misc.vars=NULL, data) {
  # Removes the misc.vars from the formula for 'data'
  
  # Set y to the dependent variable (e.g y = "Species"),
  # misc.vars are variables like id, date etc.
  # data is the data.frame
  
  if (any(!c(y, misc.vars) %in% names(data)))
    stop("Arguments not in 'data'")
  
  x <- names(data)[!names(data) %in% c(y, misc.vars)]
  fo <- as.formula(paste0(y, " ~ ", paste(x, collapse="+")),
                   env = globalenv())
  fo
}
