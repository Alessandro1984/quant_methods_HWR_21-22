# We simulated the case where there is a correlation (strong and positive) 
# between x1 and x2. What would happen to the estimate of b0 and b1 
# if there was no correlation between x1 and x2?
rm(list = ls())

set.seed(1234567)

# Number of observations in each sample
n <- 1000

# Number of samples
r <- 10000

# Population parameters
b0 <- 1; b1 <- 0.5; b2 <- 2.00

x1 <- rnorm(n, 4, 1)
x2 <- rnorm(n, 0, 20)

cor(x1, x2)

# Placeholders for the estimated b0 and b1  
b0hat <- numeric(r)
b1hat <- numeric(r)

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 0, 2)
  # Dependent variable
  y <- b0 + b1*x1 + b2*x2 + u
  # Estimate the model
  reg <- coefficients(lm(y~x1)) # We leave out x2 but correlation is low
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x1"]
}

mean(b1hat)
mean(b0hat)