
install.packages("meerva")
library(meerva)

# Simulate logistic regression data with measurement error
simd = meerva.sim.brn(n=4000, m=400,
                      beta = c(-0.5, 0.5, 0.2, 1, 0.5) ,
                      alpha1 = c(0.95, 0.90, 0.90, 0.95) ,
                      alpha2 = c(0.98,0.94,0.95,0.95) ,
                      bx3s1 = c(0.05, 0, 0, NA, NA) ,
                      bx3s2 = c(NA,NA,NA) )

# understanding the simd object
#' simd$simd is the full dataset with x's and y's, (n = 4000 in above example)
#' simd$id_val is length 400, identifies the validation sample id's??
#' simd$id_non is length 3600, identifies the non validation sample
#' simd$x_val is matrix of predictors (rows = 400) in validation set (not incld intercept)
#' simd$y_val is vector of length 400 for the outcome in validation set
#' simd$xs_val is matrix of surrogate predictors in validation set
#' simd$ys_val is vector of surrogate predictors in validation set
#' simd$xs_non is matrix of surrogate predictors in non validation set
#' simd$ys_non is vector of surrogate outcome in non validation set

# Read the simulated data to input data format
x_val = simd$x_val
y_val = simd$y_val
xs_val = simd$xs_val
ys_val = simd$ys_val
xs_non = simd$xs_non
ys_non = simd$ys_non

# Analyze the data
brn.me = meerva.fit(x_val, y_val, xs_val, ys_val, xs_non, ys_non)
summary(brn.me)


# Simulation study for logistic reg data with
# differential misclassification in outcome
# and a predictor and measurement error in
# another predictor. nsims=10 is as an
# example only. Try running nsims=100 or
# 1000, but be prepared to wait a little while.
sim.binomial = meerva.sim.block(simfam="binomial",
                                nsims=10, seed=0, n=4000, m=400,
                                beta = c(-0.5, 0.5, 0.2, 1, 0.5) ,
                                alpha1 = c(0.95, 0.90, 0.90, 0.95),
                                alpha2 = c(0.98,0.98,0.95,0.95),
                                bx3s1=c(0.05, 0, 0, NA, NA) ,
                                bx3s2 = c(NA,NA,NA) ,
                                vmethod=2, jksize=0, compare=1)
plot(sim.binomial)
summary(sim.binomial, 1)

