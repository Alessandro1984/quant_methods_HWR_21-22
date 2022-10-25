########################################################
########## OLS regression using matrix algebra #########
########################################################
rm(list = ls())

library(readr)

Cobb <- read_delim("Additional materials/Basics of matrix algebra/Cobb.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Let's transform the variables using logarithms:
Cobb$lnGDP <- log(Cobb$GDP)
Cobb$lnLABOR <- log(Cobb$LABOR)
Cobb$lnCAPITAL <- log(Cobb$CAPITAL)

# Let's run a simple regression of a Cobb-Douglas production function
Reg1 <- lm(lnGDP ~ lnCAPITAL + lnLABOR, data = Cobb)
summary(Reg1)

# Now we want to replicate the results obtained using the command lm() implementing
# the matrix algebra operations
# The OLS regression equation is Y = Xb + e
# where the vector of estimated coefficients is 
# b_hat = (X'X)^-1 X'Y

# Let's create the matrix of independent variables X; do not forget to include
# a column of 1s for the intercept
X <- as.matrix(cbind(1,Cobb$lnCAPITAL,Cobb$lnLABOR))
X

# Let's create the vector of the dependent variable Y
Y <- as.matrix(Cobb$lnGDP)
Y

# Let's calculate the coefficients and compare them with the results above
b_hat <- round(solve(t(X)%*%X)%*%t(X)%*%Y, digits = 5)
b_hat

# Let's now calculate the standard errors. 
# We need:
# 1) the vector of residuals e = Y - Y_hat = Y - X * b_hat
# 2) the variance covariance matrix (e'e) / (n-k) * (X'X)^-1

# Residuals
res <- as.matrix(Cobb$lnGDP-b_hat[1]-b_hat[2]*Cobb$lnCAPITAL-b_hat[3]*Cobb$lnLABOR)

n <- nrow(X)
k <- ncol(X)

# Variance covariance matrix
VCV <- (as.numeric(t(res)%*%res)/(n-k)) * solve(t(X)%*%X)

# Let's now calculate the standard errors taking the square root of the diagonal
# elements of the VCV matrix
se <- round(sqrt(diag(VCV)), digits = 4)

# Let's now calculate the t-statistics
tstat <- round(b_hat / se, digits = 4)
tstat

# Let's now calculate the p-values
pvalue <- round(2*pt(abs(tstat), df = n - k, lower.tail = FALSE), digits = 4)
pvalue

# Let's now put everything together into a nice looking table with labels
regression <- as.data.frame(cbind(b_hat, se, tstat, pvalue))
names(regression) = c("Estimate","Std. Error","t-Statistics","p-Value")
rownames(regression) = c("Intercept", "lnCAPITAL", "lnLABOR")

# Here the final result!
regression
