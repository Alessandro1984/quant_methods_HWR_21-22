library(wooldridge)
library(dplyr)

# With this session we replicate and extend the exercise proposed by Florian Heiss 
# in the book 'Using R for Introductory Econometrics' Chapter 2 pp. 84-90

set.seed(1234567)

# Number of observations in each sample
n <- 1000

# Number of samples
r <- 10000

# Population parameters
b0 <- 1; b1 <- 0.5

# Placeholders for the estimated b0 and b1  
b0hat <- numeric(r)
b1hat <- numeric(r)

u <- rnorm(n, 0, 2)
x <- rnorm(n, 4, 1)
y <- b0 + b1*x + u

# Single linear regression (one sample) ----
(olsres <- lm(y ~ x))

# Independent variable. The xs are fixed in repeated samples,
# we are conditioning on the values of the x therefore we must define 
# x outside the body of the loop. This assumption will be relaxed
# later (Chapter, 5 pp. )
x <- rnorm(n, 4, 1)

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 0, 2)
  # Dependent variable
  y <- b0 + b1*x + u
  # Estimate the model
  reg <- coefficients(lm(y~x))
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x"]
}

# Estimate of the expected value of beta hat zero and beta hat one
# Is it similar to the population parameter b0 and b1 (1, 0.5)? Are OLS estimators 
# unbiased?
mean(b1hat)
mean(b0hat)

# Estimate of the variances
var(b1hat)
var(b0hat)