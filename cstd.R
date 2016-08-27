# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Calculates the circular equivalent of standard deviation
# THere is the circular package which has a circular sd function. However, that
# requires transforming the data to a object of class circular. It does give the
# same result as this function, though.

cstd <- function(x) {
  
  if(any(abs(x) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI'", call. = FALSE)
  }
  
  if(NROW(x) == 1) { x = t(x) }
  
  R = sqrt(sum(sin(x))^2 + sum(cos(x))^2) / NROW(x)
  y = sqrt(-2 * log(R))
  return(y)
}
