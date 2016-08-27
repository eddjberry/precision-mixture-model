# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Calculates the circular equivalent of standard deviation
# THere is the circular package which has a mean.circular function. However, that
# requires transforming the data to a object of class circular.

cmean <- function(x) {
  
  if(any(abs(x) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI'", call. = FALSE)
  }
  
  y = atan2(sum(sin(x)), sum(cos(x)))
  return(y)
}
