# Test JV10_error

library(dplyr)
source("mixture_model_functions.R")

#errors = rnorm(100, mean = 2, sd = 15)

Tg1 = 1:90

X1 = 4:93

NT = matrix(sample(1:180, 180, replace = T), ncol = 2)

JV10_fit(X = X1, Tg = Tg1)


d <- read.csv("precision_pilot_19_06.csv")

one_item <- d %>%
  filter(test_stage == "test_1_item")
  #mutate(res_rad = wrap(response_ori),
         #tar_rad = wrap(target_ori))


X = one_item$response_ori
Tg = one_item$target_ori


JV10_fit(X = X, Tg  = Tg)


