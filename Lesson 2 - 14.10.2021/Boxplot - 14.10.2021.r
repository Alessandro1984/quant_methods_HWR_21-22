# Clear the workspace
rm(list = ls())

# Outliers in boxplots

# Data
a <- c(1,2,3,3,3,4,5,5,6,8,8,9,10,12,12,13,14,15,16,18)
b <- c(1,2,3,3,3,4,5,5,6,8,8,9,10,12,12,13,14,15,16,30)

# Plot the boxplot of a and b
boxplot(a, b,
        names = c("a", "b"),
        horizontal = TRUE)

# We define the boundaries for the identification of an outlier in the boxplot
lower_limit <- quantile(b, prob = 0.25) - 1.5 * IQR(b)
lower_limit

upper_limit <- quantile(b, prob = 0.75) + 1.5 * IQR(b)
upper_limit
