# Clear the workspace
rm(list = ls())

x <- c(-3, 3, 5) # Variable x
y <- c(5, 6, 2) # Variable y

# Plot
plot(x, y,             # First variable on the h. axis then the variable on the v. axis
     xlim = c(-4,6),     # Length of the x-axis
     ylim = c(-4, 10))   # Length of the y-axis
abline(v = 0)            # Line for the vertical axis
abline(h = 0)            # Line for the horizontal axis

# Plotting the line of best fit
abline(lm(y ~ x),        # Slope and intercept calculated automatically with the lm() function
       col = "red")      # Color of the line

# Line for the mean of y
abline(h = mean(y),
       col = "green",
       lty = 3)

legend("bottomright",                       # Position of the legend
       title = "LEGEND",                    # Title of the legend
       expression(hat(y[i]) == hat(beta)["0"] + hat(beta)["1"]%.%x[i], bar(y)),          # Names of the variables
       col = c("red", "green"),             # Colors of the lines
       lty = c(1, 3),                       # Type of lines
       cex = 1,                             # Dimensions
       horiz = TRUE,                        # Option horizontal
       bg = "transparent",                  # Background
       bty = "n")                           # No box around the legend

# Slope coefficient
beta_1 <- cov(y, x) / var(x)

# Intercept
beta_0 <- mean(y) - beta_1 * mean(x)

# We calculate the fitted values
y_hat <- beta_0 + beta_1 * (x)
y_hat

# We calculate the residuals
u_hat <- y - y_hat
u_hat

# We calculate the residual sum of squares (SSR)
SSR <- sum(u_hat^2)
SSR

# We calculate the explained sum of squares (SSE)
SSE <- sum((y_hat-mean(y))^2)
SSE

# We can calculate the sum of square total (SST)
SST <- SSR + SSE
SST

# Let's check
var(y)*(length(y)-1)

# We can finally calculate the R2
R2 <- SSE / SST
R2

# Alternatively
R2 <- 1 - SSR/SST
R2

# Adjusted R2
R2_adj <- 1 - (SSR/(length(y)-1-1)) / (SST/(length(y)-1))
R2_adj

# We can also calculate the root mean squared error (RMSE)
RMSE <- sqrt(SSR/(length(y)-2)) # Degrees of freedom correction = 2
RMSE

# We can now compare our work with the output of the lm() function
model1 <- lm(y ~ x)
summary(model1)

library(stargazer)

stargazer(model1, type = "text", keep.stat = c("n", "rsq", "ser", "adj.rsq"))
