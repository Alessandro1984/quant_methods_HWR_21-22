# This exercise is taken from the book Hands-On Programming with R by Garrett Grolemund
library(ggplot2)

# Simulation of the expected value of the fair die/dice
roll_function <- function(nr_of_die) {
  die <- 1:6
  roll <- sample(die, 
                 size = nr_of_die, 
                 replace = TRUE,
                 prob = rep(1/6, 6))
  
  sum_roll <- sum(roll) # The function sum is necessary for simulations with two or more dice
  
  return(sum_roll)
}

throws <- replicate(10000, roll_function(nr_of_die = 1))

# Plot
qplot(throws,
       binwidth = 1,
       ylab = "Frequency",
       xlab = "Numbers") +
  geom_vline(aes(xintercept = mean(throws))) +
  geom_text(aes(y = 500,
                x = mean(throws),
                label = mean(throws))) +
  theme_minimal()