# Clean the workspace
rm(list = ls())

set.seed(12345)

# This short exercise aims to show that Pearson's correlation coefficient 
# measures the linear correlation between two variables and 
# cannot capture other types of non-linear relationships

# Linear type relationship
x1 <- runif(100, min = 0, max = 10)
y1 <- 10 + 2 * x1 + rnorm(100, mean = 0, sd = 5)

plot(x1, y1)
abline(lm(y1 ~ x1))
cor(x1, y1)

# The Pearson correlation coefficient is symmetric
cor(x1, y1) == cor(y1, x1)

# Non-linear relationship, e.g. sine function
x2 <- runif(100, min = 0, max = 10)
y2 <- sin(x2) + rnorm(100, mean = 0, sd = 0.2)

plot(x2, y2)
abline(lm(y2 ~ x2))
cor(x2, y2)
