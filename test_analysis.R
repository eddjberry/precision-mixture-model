
library(dplyr)

source("mixture_model_functions.R")

d <- read.csv("test_data.csv")

d1 <- d %>%
  filter(id == "pilot1")

d2 <- d %>%
  filter(id == "pilot2")


(ests1 = JV10_fit(X = d1$response, Tg  = d1$target))

(ests2 = JV10_fit(X = d2$response, Tg  = d2$target))

# For id == "pilot1": K = 0.1386, Pt = 0.9853, Pn = 0, Pu = 0.0147, LL = -164.9913
# For id == "pilot2": K = 0.5433, Pt = 0.1003, Pn = 0, Pu = 0.8997, LL = -165.3788

# looping over df; the tar.var argument actually defaults to "target" and
# res.var defaults to response but I've included them here for ease of reading.

JV10_df(d, tar.var = "target", res.var = "response")
