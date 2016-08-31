
library(dplyr)

source("mixture_model_functions.R")

d <- read.csv("test_data.csv")

d1 <- d %>%
  filter(id == "pilot1")

d2 <- d %>%
  filter(id == "pilot2")


(ests1 = JV10_fit(X = d1$response_ori, Tg  = d1$target_ori))

(ests2 = JV10_fit(X = d2$response_ori, Tg  = d2$target_ori))

# looping over df (ugly 'solution')

l <- split(d, d$id)

paras <- data.frame(K = FALSE, Pt = FALSE, Pn = FALSE, Pu = FALSE)

for(i in seq_along(l)) {
  dd <- data.frame(l[i])
  
  X <- as.matrix(dd[3])
  Tg <- as.matrix(dd[4])
  
  B <- JV10_fit(X, Tg, return.ll = FALSE)
  
  paras[i,] <- B
}
