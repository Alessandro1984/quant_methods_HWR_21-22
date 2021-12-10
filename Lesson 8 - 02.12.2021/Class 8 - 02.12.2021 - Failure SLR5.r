# Violation of homoskedasticity assumption SLR.5
# OLS estimator still unbiased but sampling variation wrong

set.seed(1234567)

# Number of observations in each sample
n <- 1000

# Number of samples
r <- 10000

# Population parameters
b0 <- 1; b1 <- 0.5

b0hat <- numeric(r)
b1hat <- numeric(r)

x <- rnorm(n, 4, 1)

# Loop
for(j in 1:r) {
  varu <- 4/exp(4.5) * exp(x) # The variance of u depends on x
  # Error
  u <- rnorm(n, 0, sqrt(varu))
  # Dependent variable
  y <- b0 + b1*x + u
  # Estimate the model
  reg <- coefficients(lm(y~x))
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x"]
}

mean(b1hat)
mean(b0hat)

# What happens to the estimate of the variances?
var(b1hat)
var(b0hat)
