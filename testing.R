# Test JV10_error

library(dplyr)
library(purrr)
source("mixture_model_functions.R")

#errors = rnorm(100, mean = 2, sd = 15)

# Tg1 = 1:90
# 
# X1 = 4:93
# 
# NT = matrix(sample(1:180, 180, replace = T), ncol = 2)
# 
# JV10_fit(X = X1, Tg = Tg1)


d <- read.csv("precision_pilot_19_06.csv")

one_item <- d %>%
  filter(test_stage == "test_1_item")
  #mutate(res_rad = wrap(response_ori),
         #tar_rad = wrap(target_ori))

three_item <- d %>%
  filter(test_stage == "test_3_item")
#mutate(res_rad = wrap(response_ori),
#tar_rad = wrap(target_ori))

p1 <- three_item %>%
  filter(UPN == "pilot2")

X = p1$response_ori; Tg  = p1$target_ori

(ests = JV10_fit(X = X, Tg  = Tg))


(ests2 = JV10_function(X, Tg))

B = ests$B

B2 = ests$B

map_dbl(B2, round, digits = 4)

matlab = list(p1_fit = c(0.1386, 0.9853, 0, 0.0147, -164.9913), 
               p1_function = c(0.3641, 0.3344, 0, 0.6656, -165.0413),
               p2_fit = c(0.5425, 0.1003, 0, 0.8997, -165.3788),
               p2_function = c(0.5425, 0.1004, 0, 0.8996, -165.3787))
  
  
  
max_dLL = 10^-4; dLL = 0.2; max_iter = 10^4; iter = 1

(abs(dLL) > max_dLL |  (iter < max_iter))
