# In this exercise, we are going to program a function that 
# calculates the standard deviation of some input vector. We will then compare the result 
# with the result obtained with the in-built function R sd()

a <- c(3, 5, 7, 8 ,9) 

std_dev <- sqrt(1/(length(a) - 1) * sum((a - mean(a))^2))
std_dev

my_std_dev <- function(x) {
  
  std_dev <- sqrt(1/(length(x) - 1) * sum((x - mean(x))^2)) 
  
  return(std_dev)
  
}

# Let's test the function
my_std_dev(x = a)

# We can now compare our result with the output of the function sd()
sd(a) == my_std_dev(a)
 
