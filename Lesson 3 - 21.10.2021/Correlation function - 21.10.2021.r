# Clean the workspace
rm(list = ls())

set.seed(12345)

# Programming challenge:

# Program a function that takes two input variables and 
# returns the Pearson correlation coefficient between the two

my_corr_function <- function(x, y) {
  
  numerator <- sum((x - mean(x)) * (y - mean(y)))
    
  denominator <- sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2))
    
  correl <- numerator / denominator
  
  return(correl)
  
}

# Input variables
a <- rnorm(100, mean = 0, sd = 1)
b <- rnorm(100, mean = 0, sd = 5)

# Test the function
my_corr_function(x = a, y = b)

# We can finally compare our result with the output of the function cor()
round(my_corr_function(x = a, y = b), digits = 10) == round(cor(a, b), digits = 10)
# PS. Note that we must slightly round the results to be sure that numbers 
# obtained with my_corr_function() and the in-built cor() function are
# exactly equal
