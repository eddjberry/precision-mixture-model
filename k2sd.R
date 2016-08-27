# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Returns the standard deviation of a wrapped normal distribution
# corresponding to a Von Mises concentration parameter of K

# Ref: Topics in Circular Statistics, S. R. Jammalamadaka & A. Sengupta

k2sd <- function(K){
  if(K == 0){
    S = Inf
    } else if(is.infinite(K)){
      S = 0
      } else {
        S = sqrt(-2 * log(besselI(K, 1) / besselI(K, 0)))
      }
  return(S)
}



