# Clean the workspace
rm(list = ls())

# 2)

x <- c(6,5,6,3,1,5)

# Sample mean
mean(x)

# Expected value
die <- 1:6

E_x <- sum(die * rep(1/6,6))

E_x

# 3)

pays <- c(0, 10, 0, 40, 0, -20)

E_pays <- sum(pays * rep(1/6,6))

# Expected pay of the game
E_pays

# 4)

tickets <- 100000

pays_lottery <- c(5, 25, 100, 10000)

probabilities <- c(200/tickets, 20/tickets, 5/tickets, 1/tickets)

E_pays_lottery <- sum(pays_lottery * probabilities)

# Fair price for a ticket
E_pays_lottery