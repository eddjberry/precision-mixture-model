# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Returns the probability density function for the Von Mises distribution
# with mean MU and concentration K, 
# evaluated at the values in X (given in radians)

vonmisespdf <- function(X, MU, K) {
  p = exp(K * cos(X-MU)) / (2 * pi * besselI(K, 0))
  return(p)
}
