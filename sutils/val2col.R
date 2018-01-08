val2col <- function(x) {
  
  # color.scale is from 'plotrix'.
  # small  -> teal
  # middle -> blue
  # big    -> red
  
  m <- mean(x)
  s <- sd(x)
  cyan <- x < m - 1.8*s
  red <- x > m + 1.8*s
  cent <- !(cyan || red)
  
  #qu <- quantile(x[cent], 0.5)
  colors <- character(length(x))
  #small <- x[cent] < qu
  
  #colors[cent & small] <- color.scale(x[cent & small], cs1=c(0.5, 0), 
  #                                       cs2=c(1, 0), 
  #                                       cs3=c(1, 1))
  #colors[cent & !small] <- color.scale(x[cent & !small], cs1=c(0, 1),
  #                                         cs2=c(0, 0),
  #                                         cs3=c(1, 0))
  
  colors[cent] <- color.scale(x[cent], cs1=c(0, 1),
                                       cs2=c(0, 0),
                                       cs3=c(1, 0))
  colors[cyan] <- "#00ffff"
  colors[red] <- "#ff0000"
  
  colors
}
