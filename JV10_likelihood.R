# Translated into R by Ed D. J. Berry (github.com/eddjberry) 
# from functions written by Paul Bays (paulbays.com) in Matlab

source("vonmisespdf.R")

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

##########################################################################
#   Copyright 2010 Paul Bays. This program is free software: you can     #
#   redistribute it and/or modify it under the terms of the GNU General  #
#   Public License as published by the Free Software Foundation.         #
##########################################################################