# Clear the workspace
rm(list = ls())

x <- c(-3, 3, 5) # Variable x
y <- c(5, 6, 2) # Variable y

# We can fit a quadratic model; remember the general equation of the parabola f(x) = c + b*x + a*x^2
model2 <- lm(y ~ x + I(x^2))
model2

beta_0 <- unname(coef(model2)["(Intercept)"])
beta_1 <- unname(coef(model2)["x"])
beta_2 <- unname(coef(model2)["I(x^2)"])

# We use the estimated parameters to plot a parabola
mod <- function(x) {x^2 * beta_2 + x * beta_1 + beta_0}

plot(x, y, 
     xlim = c(-10, 10), 
     ylim = c(-10, 10))
curve(mod, from = -10, to = 10, add = TRUE)
abline(v = 0)            # Line for the vertical axis
abline(h = 0)            # Line for the horizontal axis
points(-3, 5, col = "black", pch = 19, cex = 1) # Mark the point in black
points(3, 6, col = "black", pch = 19, cex = 1)   # Mark the point in black
points(5, 2, col = "black", pch = 19, cex = 1)   # Mark the point in black

# We can use the estimated coefficients to simulate some new data and then fit a parabola again
# and a linear model. We then compare the R2. 
varx <- -10:10
vary <- beta_0 + rnorm(21, 0, 2) + (varx) * beta_1 + (varx)^2 * beta_2

# Estimating the new parameters (they are very similar)
model3 <- lm(vary ~ varx + I(varx^2))

# We calculate the predicted values
plot(varx, vary, xlim = c(-10, 10), ylim = c(-10, 10))
vary_new <- predict(model3)
matlines(varx, vary_new)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)

# We mark the original observations. They are very very close to the fitted model
points(-3, 5, col="black", pch = 19, cex = 1)
points(3, 6, col="black", pch = 19, cex = 1)
points(5, 2, col="black", pch = 19, cex = 1)

# We can now try to fit a linear model
model4 <- lm(vary ~ varx)

# Overlap the linear model to the previous plot
abline(model4)

# Which model has the largest R2?
library(stargazer)

stargazer(model4, model3, type = "text", keep.stat = c("n", "rsq", "ser"))
