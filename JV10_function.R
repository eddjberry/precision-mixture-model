# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Ref: Bays PM, Catalao RFG & Husain M. The precision of visual working 
# memory is set by allocation of a shared resource. Journal of Vision 
# 9(10): 7, 1-11 (2009)

# Returns a list where the first element in a single row data frame
# of parameter estimates and the second is a single log likelihood value


# repmat function adapted from http://haky-functions.blogspot.co.uk/2006/11/repmat-function-matlab.html

repmat = function(X, m, n){
  mx = NROW(X)
  nx = NCOL(X)
  matrix(data = X, nrow = mx, ncol = nx*n)
}

#=============

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

#=============

source("wrap.R")
source("vonmisespdf.R")

#=============

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
  
  if(nn > 0) {
    NE = repmat(X, 1, nn) - NT
  } else {
    NE = repmat(X, 1, nn) }
  
  LL = NaN; dLL = NaN; iter = 0
  
  while((any(abs(dLL) > max_dLL) | (iter < max_iter))) {
    
    iter = iter + 1
    
    Wt = Pt * vonmisespdf(E, 0, K)
    Wg = Pu * replicate(n, 1) / (2 * pi)
    
    if(nn == 0) {
      Wn = matrix(0, nrow = NROW(NE), ncol = NCOL(NE))
    } else {
      Wn = Pn/nn * vonmisespdf(NE, 0, K)
    }
    
    W = rowSums(cbind(Wt, Wg, Wn))
    
    dLL = LL - sum(log(W))
    
    LL = sum(log(W))
    
    Pt = sum(Wt / W) / n
    Pn = sum(rowSums(Wn) / W) / n
    Pu = sum(Wg / W) / n
    
    rw = c((Wt / W), (Wn / repmat(W,1,nn)))
    
    S = c(sin(E), sin(NE)) ; C = c(cos(E), cos(NE))
    r = c(sum(sum(S * rw)), sum(sum(C * rw)))
    
    if(sum(sum(rw)) == 0) {
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


##########################################################################
#   Copyright 2010 Paul Bays. This program is free software: you can     #
#   redistribute it and/or modify it under the terms of the GNU General  #
#   Public License as published by the Free Software Foundation.         #
##########################################################################