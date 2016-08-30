
library(dplyr)

source("mixture_model_functions.R")

d <- read.csv("test_data.csv")

d1 <- d %>%
  filter(id == "pilot1")

d2 <- d %>%
  filter(id == "pilot2")

d3 <- d %>%
  filter(id == "pilot3")


(ests1 = JV10_fit(X = d1$response_ori, Tg  = d1$target_ori))

(ests2 = JV10_fit(X = d2$response_ori, Tg  = d2$target_ori))

(ests3 = JV10_fit(X = d3$response_ori, Tg  = d3$target_ori))


# For id == "pilot1": K = 0.1386, Pt = 0.9853, Pn = 0, Pu = 0.0147, LL = -164.9913
# For id == "pilot2": K = 0.5433, Pt = 0.1003, Pn = 0, Pu = 0.8997, LL = -165.3788
# For id == "pilot3": K = 3.2230, Pt = 0.0004, Pn = 0, Pu, 0.9996, LL = -165.4104
  

  