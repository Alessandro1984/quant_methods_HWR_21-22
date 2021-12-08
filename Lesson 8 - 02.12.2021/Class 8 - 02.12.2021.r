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

# Independent variable (the xs are fixed in repeated samples,
# we are conditioning on the values of the x)
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

# Expected value of the error term not zero ----
# We now want to simulate the violation of the assumption that 
# the expected value of the error term is zero. We assume that the value 
# is no longer zero but four while all the rest remains equal. Which of the two coefficients will be affected 
# by this change?

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 4, 2)
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

# Violation of the zero conditional mean assumption SLR.4 ----
# Both OLS coefficients will be biased

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

# Violation of homoskedasticity assumption SLR.5 ----
# OLS estimator still unbiased but sampling variation wrong

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

# Estimate of the variances
var(b1hat)

var(b0hat)

# Omitted variable bias ----
# We now want to use the same Monte Carlo procedure to simulate
# the case of omitted variable bias

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

cor(x1, x2) # not surprisingly, x1 and x2 are positively correlated

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

mean(b1hat)
mean(b2hat)
mean(b0hat)

# We now simulate the case of an important variable omitted from the regression
# Are OLS estimators still unbiased?

# Loop
for(j in 1:r) {
  # Error
  u <- rnorm(n, 0, 2)
  # Dependent variable
  y <- b0 + b1*x1 + b2*x2 + u
  # Estimate the model with x2
  reg <- coefficients(lm(y~x1)) # We leave out x2
  # Filling the placeholders
  b0hat[j] <- reg["(Intercept)"]
  b1hat[j] <- reg["x1"]
  #b2hat[j] <- reg["x2"]
}

mean(b1hat)
#mean(b2hat)
mean(b0hat)

# Question: We simulated the case where there is a correlation (strong and positive) 
# between x1 and x2. What would happen to the estimate of b0 and b1 
# if there was no correlation between x1 and x2? Try:
# x2 <- rnorm(n, 0, 20)

# Question: What if we were to include an irrelevant variable in the regression? 
# (this means that the population parameter would be zero) 