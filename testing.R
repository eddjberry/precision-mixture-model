# Test JV10_error

#errors = rnorm(100, mean = 2, sd = 15)

Tg = 1:100

X = 10:109

#NT = matrix(sample(1:180, 300, replace = T), ncol = 3)

JV10_fit(X = X, Tg = Tg)
