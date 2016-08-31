# Below are the core functions for doing the mixture modelling for precision data

# Just source this script and you'll have everything you need to work through the README

# Unless otherwise stated translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Ref: Bays PM, Catalao RFG & Husain M. The precision of visual working 
# memory is set by allocation of a shared resource. Journal of Vision 
# 9(10): 7, 1-11 (2009)

#==============================================================================

# This function maps the input values (y) onto the circular space -pi to pi.
# If the inputs are in degrees then divide by 180*pi. However, if the degrees
# are in the range 180 to 0 (e.g. bar orientations) then divide by 90*pi

wrap <- function(Y, bound = pi) {
  X <- ((Y + bound) %% (bound*2)) - bound
  return(X)
}

#==============================================================================

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

#==============================================================================

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

#==============================================================================

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

#==============================================================================

# Returns the probability density function for the Von Mises distribution
# with mean MU and concentration K, 
# evaluated at the values in X (given in radians)

vonmisespdf <- function(X, MU, K) {
  p = exp(K * cos(X-MU)) / (2 * pi * besselI(K, 0))
  return(p)
}

#==============================================================================

# logspace function for logarithmically spaced vectors
# soure: http://r.789695.n4.nabble.com/logarithmic-seq-tp900431p900433.html

logspace <- function(a, b, n) exp(log(10)*seq(a, b, length.out=n))

# trapz function from the caTools package by Jarek Tuszynski

trapz <- function(x, y) {
  idx = 2:length(x)
  return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

#==============================================================================

JV10_error <- function(X, Tg = 0) {
  if(any(abs(X) > pi) | any(abs(Tg) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI", call. = FALSE)
  }
  if(any(dim(X)) != any(dim(Tg))) {
    stop("Error: Inputs must have the same dimensions", call. = FALSE)
  }
  if(NROW(X) == 1) { X = t(X); Tg = t(Tg) }
  
  E = wrap(X - Tg) # error: diff between response and target
  
  # Precision
  N = NROW(X)
  
  x = logspace(-2, 2, 1000)
  P0 = trapz(x, N / sqrt(x) %*% exp(x + N %*% exp(-x))) # expected precision under uniform distribution
  
  P = (1 / cstd(E)) - P0
  
  # Bias
  B = cmean(E)
  
  return(data.frame(precision = P, bias = B))
}

#==============================================================================

JV10_fit <- function(X, Tg, NT = replicate(NROW(X), 0), return.ll = TRUE) {
  if(NCOL(X) > 2 | NCOL(Tg) > 1 | NROW(X) != NROW(Tg) | (any(NT != 0) & NROW(NT) != NROW(X) | NROW(NT) != NROW(Tg))) {
    stop("Error: Input not correctly dimensioned", call. = FALSE)
  }
  n = NROW(X)
  
  nn = ifelse(any(NT != 0), NCOL(NT), 0)
  
  # Start parameters
  K = c(1, 10, 100)
  N = c(0.01, 0.1, 0.4)
  U = c(0.01, 0.1, 0.4)
  
  if(nn == 0) {N = 0}
  
  loglik = -Inf
  
  # Parameter estimates
  for(i in seq_along(K)) {
    for(j in seq_along(N)) {
      for(k in seq_along(U)) {
        est_list = JV10_function(X = X, Tg = Tg, NT = NT, B_start = c(K[i], 1-N[j]-U[k], N[j], U[k]))
        if (est_list$ll > loglik & !is.nan(est_list$ll) ) {
          loglik = est_list$ll
          B = est_list$b
        }
      }
    }
  }
  
  if(return.ll == TRUE) {
    return(list(B = B, LL = loglik))
  } else {
    return(B)
  }
}

#==============================================================================

# repmat function adapted from http://haky-functions.blogspot.co.uk/2006/11/repmat-function-matlab.html

repmat = function(X, nn){
  mx = NROW(X)
  nx = NCOL(X)
  if(nn > 0){
    return(matrix(data = X, nrow = mx, ncol = nx*nn))
  } else {
  return(matrix(nrow = mx, ncol = nn))
  }
}

#==================

# Inverse of A1 function. This is available in the circular package but I have
# recreated it here from Bays' code

A1inv <- function(R) {
  if(0 <= R & R < 0.53) {
    K = 2 * R + R^3 + (5 * R^5) / 6
  } else if(R < 0.85) {
    K = -0.4 + 1.39 * R + 0.43 / (1 - R)
  } else {
    K = 1 / (R^3 - 4 * R^2 + 3 * R)
  }
  return(K)
}
#==================

# Returns a list where the first element in a single row data frame
# of parameter estimates and the second is a single log likelihood value

JV10_function <- function(X, Tg, 
                          NT = replicate(NROW(X), 0), 
                          B_start = NULL) {
  
  if(NCOL(X) > 2 | NCOL(Tg) > 1 | NROW(X) != NROW(Tg) | (any(NT != 0) & NROW(NT) != NROW(X) | NROW(NT) != NROW(Tg))) {
    stop("Error: Input not correctly dimensioned", call. = FALSE)
  }
  
  if((!(is.null(B_start))) & (any(B_start[1] < 0, B_start[2:4] < 0, B_start[2:4] > 1, abs(sum(B_start[2:4]) - 1) > 10^-6))) {
    stop("Error: Invalid model parameters", call. = FALSE)
  }
  
  max_iter = 10^4; max_dLL = 10^-4
  
  n = NROW(X)
  
  nn = ifelse(any(NT != 0), NCOL(NT), 0)
  
  # Default starting parameter
  
  if(is.null(B_start)) {
    K = 5; Pt = 0.5
    Pn = ifelse(nn > 0, 0.3, 0)
    Pu = 1 - Pt - Pn
  } else {
    K = B_start[1]; Pt = B_start[2]
    Pn = B_start[3]; Pu = B_start[4]
  }
  
  E = wrap(X - Tg)
  
  if(nn > 0){
    NE = wrap(repmat(X, nn) - NT)
  } else {
    NE = repmat(X, nn)
  }
  
  LL = 0; dLL = 1; iter = 1
  
  while(TRUE) {
    iter = iter + 1
    
    Wt = Pt * vonmisespdf(E, 0, K)
    Wg = Pu * replicate(n, 1) / (2 * pi)
    
    if(nn == 0){
      Wn = matrix(nrow = NROW(NE), ncol = NCOL(NE)) 
    } else {
      Wn = Pn/nn * vonmisespdf(NE, 0, K)
    }
                
    W = rowSums(cbind(Wt, Wg, Wn))
    
    dLL = LL - sum(log(W))
    LL = sum(log(W))
    
    if(abs(dLL) < max_dLL | iter > max_iter | is.nan(dLL)) {
      break
    }
      
    Pt = sum(Wt / W) / n
    Pn = sum(rowSums(Wn) / W) / n
    Pu = sum(Wg / W) / n
    
    rw = c((Wt / W), (Wn / repmat(W, nn)))
    
    S = c(sin(E), sin(NE)) ; C = c(cos(E), cos(NE))
    r = c(sum(sum(S * rw)), sum(sum(C * rw)))
      
    if(sum(sum(rw, na.rm = T)) == 0) {
      K = 0
    } else {
      R = sqrt(sum(r^2)) / sum(sum(rw))
      K = A1inv(R)
    }
      
    if(n <= 15) {
      if(K < 2) {
        K = max(K - 2 / (n * K), 0)
      } else {
        K = K * (n - 1)^3 / (n^3 + n)
      }
    }
}
 
  
  if(iter > max_iter) {
    warning('JV10_function:MaxIter','Maximum iteration limit exceeded.', call. = FALSE)
    B = c(NaN, NaN, NaN, NaN); LL = NaN
  } else {
    B = data.frame(K = K, Pt = Pt, Pn = Pn, Pu = Pu)
  }
  
  return(list(b = B, ll = LL))
  
}

#==============================================================================
  
JV10_likelihood <- function(B, X, Tg, NT = replicate(NROW(X), 0)) {
  if(NCOL(X) > 2 | NCOL(Tg) > 1 | NROW(X) != NROW(Tg) | (any(NT != 0) & NROW(NT) != NROW(X) | NROW(NT) != NROW(Tg))) {
    stop("Error: Input not correctly dimensioned", call. = FALSE)
  }
  if(B_start[1] < 0 | any(B_start[2:4] < 0) | any(B_start[2:4] > 1) | abs(sum(B_start[2:4]) - 1) > 10^-6) {
    stop("Error: Invalid model parameters")
  }
  
  n = NROW(X)
  
  E = wrap(X - Tg)
  
  Wt = B[2] * vonmisespdf(E, 0, B[1])
  Wu = B[4] * matrix(1, nrow = n, ncol = 1) / (2 * pi)
  
  if(any(NT != 0)) {
    L = rowSums(cbind(Wt, Wu))
  } else {
    nn = NCOL(NT)
    NE = wrap(repmat(X, 1, nn) - NT)
    Wn = B[3] / nn * vonmisespdf(NE, 0, B[1])
    L = rowSums(cbind(Wt, Wn, Wu))
  }
  
  LL = sum(log(L))
  
  return(data.frame(LL = LL, L = L))
  
}

#==============================================================================

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

#==============================================================================

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

#==============================================================================

# This (admittedly ugly) function takes a dataframe and returns parameters
# estimates for each level of the id variable and the given
# target and response variables.

# If there are non-target variables then nt.vars should be either a list column 
# names or a character vectorsi.e. nt.vars = c("nt1", "nt2")

# This function is by Ed Berry, I attribute none of its ugliness to Paul Bayes :')

JV10_df <- function(df, id.var = "id", tar.var = "target", res.var = "response", nt.vars = NULL){
  id <- d[, id.var]
  
  l <- split(d, id)
  
  paras <- data.frame(id = FALSE, K = FALSE, Pt = FALSE, Pn = FALSE, Pu = FALSE)
  
  for(i in seq_along(l)) {
    df <- as.data.frame.list(l[i], col.names = colnames(l[i]))
    
    X <- as.matrix(df[, tar.var])
    Tg <- as.matrix(df[res.var])
    
    if(is.null(nt.vars)) {
      B <- JV10_fit(X, Tg, return.ll = FALSE)
    } else {
      NT = as.matrix(df[,nt.vars])
      B <- JV10_fit(X, Tg, NT, FALSE)
    }
    id <- as.character(df[1, id.var])
    paras[i, 1] <- id
    paras[i,2:5] <- B
  }
  return(paras)
}

  
##########################################################################
#   Copyright 2010 Paul Bays. This program is free software: you can     #
#   redistribute it and/or modify it under the terms of the GNU General  #
#   Public License as published by the Free Software Foundation.         #
##########################################################################