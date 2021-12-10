# We now want to simulate the violation of the assumption that 
# the expected value of the error term is zero. We assume that the value 
# is no longer zero but four while all the rest remains equal.

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
  u <- rnorm(n, 4, 2) # Mean = 4
  # Dependent variable
  y <- b0 + b1*x + u
  # Estimate the model
  reg <- coefficients(lm(y~x))
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x"]
}

# Where is the bias showing up?
mean(b1hat)
mean(b0hat)
