# Violation of the zero conditional mean assumption SLR.4
# Both OLS coefficients will be biased

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
  # Error
  u <- rnorm(n, (x-4)/5, 2) # The error term is a function of x
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
