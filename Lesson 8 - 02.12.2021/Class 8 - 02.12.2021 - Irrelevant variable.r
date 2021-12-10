# Question: What if we were to include an irrelevant variable in the regression? 
# This means that the population parameter for this particular variable would be zero
rm(list = ls())

set.seed(1234567)

# Number of observations in each sample
n <- 1000

# Number of samples
r <- 10000

# Population parameters
b0 <- 1; b1 <- 0.5

x1 <- rnorm(n, 4, 1)
x2 <- rnorm(n, 0, 1)

b0hat <- numeric(r)
b1hat <- numeric(r)
b2hat <- numeric(r)

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 0, 2)
  # Dependent variable
  y <- b0 + b1*x1 + u # No x2
  # Estimate the model
  reg <- coefficients(lm(y~x1+x2)) # We include an irrelevant variable x2
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x1"]
  b2hat[j] <- reg["x2"]
}

# Unbiased
mean(b0hat)
mean(b1hat)
mean(b2hat)