library(stargazer)

set.seed(12345)

x <- runif(100, min = 0, max = 100) # Values of the x
u <- rnorm(100, 0, 10)              # Error term
beta_0 <- 2                         # Intercept
beta_1 <- 0.5                       # Slope
c <- 10                             # Constant value for the modification of the variables

# The simulated theoretical model
y <- beta_0 + beta_1 * x + u

# Plot
plot(x, y)
abline(lm(y ~ x))

# First regression
reg1 <- lm(y ~ x)

# Modification of y-values
y2 <- y * c

reg2 <- lm(y2 ~ x)

# Modification of x-values
x2 <- x * c

reg3 <- lm(y ~ x2)

# Regressions table
stargazer(reg1, reg2, reg3, type = "text", keep.stat = c("n", "rsq", "ser"))

