# logspace function for logarithmically spaced vectors
# soure: http://r.789695.n4.nabble.com/logarithmic-seq-tp900431p900433.html

logspace <- function(a, b, n) exp(log(10)*seq(a, b, length.out=n))

# trapz function from the caTools package by Jarek Tuszynski

trapz <- function(x, y) {
  idx = 2:length(x)
  return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

# Ref: Bays PM, Catalao RFG & Husain M. The precision of visual working 
# memory is set by allocation of a shared resource. Journal of Vision 
# 9(10): 7, 1-11 (2009)

# Returns a one-row data frame with precision and bias for circular data

# The inputs should both be single column vectors.
# X is the response in radians and T is the target orienation in radians
# T defaults to 0 if not specified

# The functions wrap() and cstd() need to be available in your working directory
# or global environment

source("wrap.R")
source("cstd.R")
source("cmean.R")

#=============

JV10_error <- function(X, Tg = 0) {
  if(any(abs(X) > pi) | any(abs(Tg) > pi)) {
    stop("Error: Input values must be in radians, range -PI to PI", call. = FALSE)
  }
  if(any(dim(X)) != any(dim(Tg))) {
    stop("Error: Inputs must have the same dimensions", call. = FALSE)
  }
  
  E = wrap(X - Tg) # error: diff between response and target
  
  # Precision
  N = NROW(X)
  
  x = logspace(-2, 2, 100)
  P0 = trapz(x, N / (sqrt(x) * exp(x + (N * exp(-x))))) # expected precision under uniform distribution
  
  P = (1 / cstd(E)) - P0
  
  # Bias
  B = cmean(E)
  
  return(data.frame(precision = P, bias = B))
}

##########################################################################
#   Copyright 2010 Paul Bays. This program is free software: you can     #
#   redistribute it and/or modify it under the terms of the GNU General  #
#   Public License as published by the Free Software Foundation.         #
##########################################################################