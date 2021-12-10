# Omitted variable bias ----
# We now want to use the same Monte Carlo procedure to simulate
# the case of omitted variable bias
rm(list = ls())

set.seed(1234567)

# Number of observations in each sample
n <- 1000

# Number of samples
r <- 10000

# Population parameters
b0 <- 1; b1 <- 0.5; b2 <- 2

b0hat <- numeric(r)
b1hat <- numeric(r)
b2hat <- numeric(r)

x1 <- rnorm(n, 4, 1)
x2 <- x1 + rnorm(n, 0, 1) # x2 is basically x1 plus some noise

cor(x1, x2) # Not surprisingly, x1 and x2 are positively correlated

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 0, 2)
  # Dependent variable
  y <- b0 + b1*x1 + b2*x2 + u
  # Estimate the model with x2
  reg <- coefficients(lm(y~x1+x2))
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x1"]
  b2hat[j] <- reg["x2"]
  
}

# Unbiased
mean(b1hat)
mean(b2hat)
mean(b0hat)

# We now simulate the case of an important variable omitted from the regression
# Are OLS estimators still unbiased?

b0hat_ov <- numeric(r)
b1hat_ov <- numeric(r)

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 0, 2)
  # Dependent variable
  y <- b0 + b1*x1 + b2*x2 + u
  # Estimate the model
  reg <- coefficients(lm(y~x1)) # We leave out x2
  # Filling the placeholders
  b0hat_ov[j] <- reg["(Intercept)"]
  b1hat_ov[j] <- reg["x1"]
}

# Biased
mean(b0hat_ov)
mean(b1hat_ov)
