# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Generates N random samples from a Von Mises distribution with 
# mean MU and concentration K

# Ref: Best, D. and Fisher, N. (1979). Applied Statistics, 24, 152-157.

randomvonmises <- function(N, MU, K) {
  if(K == 0) {
    X = (runif(N) * 2 - 1) * pi
  }
  a = 1 + (1 + 4 * (K^2))^0.5
  b = (a - (2 * a)^0.5) / (2 * K)
  r = (1 + b^2) / (2 * b)
  
  obs = 1
  
  while(obs <= N) {
    z = cos(pi * runif(1))
    f = (1 + r * z) / (r + z)
    c = K * (r - f)
    U = runif(1)
    
    if((c * (2 - c) - U > 0) | (log(c/U) + 1 - c >= 0)) {
      X[obs] = wrap(sign(runif(1) - 0.5) * acos(f) + MU)
      obs = obs + 1
    }
  }
  return(X)
}
