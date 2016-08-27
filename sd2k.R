# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Returns the Von Mises concentration parameter K corresponding
# to a standard deviation S of a wrapped normal distributions

# Ref: Topics in Circular Statistics, S. R. Jammalamadaka & A. Sengupta

sd2k <- function(S){
  
  R = exp(-S^2 / 2)
  
  K = 1 / R^3 - 4 * R^2 + 3 * R
  
  K[R < 0.85] = -0.4 + 1.39 * R[R < 0.85] + 0.43 / (1 - R[R < 0.85])
  
  K[R < 0.53] = 2 * R[R < 0.53] + R[R < 0.53]^3 + (5 * R[R < 0.53]^5) / 6
  
  return(K)
}
