# Clear the workspace
rm(list = ls())

######### Part II #########

# 1) Find mean and median
x <- c(3,5,2,6,5,9,5,2,8,5)

mean(x)

median(x)

# 2) Find the weighted arithmetic mean

# Values of z
z <- c(70,90,85)

# Weights
wt <- c(1,1,3)

weighted.mean(z, wt)

########## Part III ##########

# A percentile is a point in a distribution at which or below which a given proportion of data is found. 
# The k-th percentile divides the data in a way that k-percent of the data lie below the percentile
# and (100 - k)-percent lie above the percentile. It is also common to hear about quantiles. 
# Quantiles is a more generic terms which indicates values partitioning data in equally spaced groups.
# Specific types of quantiles are percentiles (see above), deciles, quartiles, etc.
a <- c(1,2,3,3,3,4,5,5,6,8,8,9,10,12,12,13,14,15,16,18)

# 1) Find the values existing at the Q_1, Q_2, Q_3 and Q_4, where Q stands for quartile
quantile(a, probs = c(0.25, 0.50, 0.75, 1))

# Do we obtain the same results using the nearest rank method?

# n = P / 100 * N
# n <- ordinal rank
# P <- percentile of interest
# N <- number of values in the set

# Q_1, Q_2, Q_3 and Q_4
P <- c(0.25, 0.50, 0.75, 1) * 100

N <- length(a)

# Ranks of the values at Q_1, Q_2, Q_3 and Q_4
n <- P / 100 * N
n

# The values at the th percentile
a[n]

# 2) the percentile of value 4 and value 15
percentile <- ecdf(a)
percentile(4)*100
percentile(15)*100

# Let's use again the nearest rank method instead of the in-built ecdf function

# P = n / N * 100

# Let's first find the rank (n) of the value 4 and 15 by looking at the original set
P_new <- c(6, 18) / N * 100
P_new

# 3) Boxplot
boxplot(a, horizontal = TRUE)

######### Part IV #########

# 1) Find variance and standard deviation
b <- c(2, 5, 3, 4, 6)

var(b)
sd(b)
sd(b) == sqrt(var(b))

# 2) Add 10 to data
b_plus_10 <- b + 10

var(b_plus_10)
sd(b_plus_10)

# 3) Multiply data by 10
b_times_10 <- b * 10

var(b_times_10)
sd(b_times_10)

######## Part V ########

# 1) Find covariance and Pearson's correlation coefficient between the variables X and Y 

X <- c(2, 1, 3)  
Y <- c(10, 30, 50)   

cov(X, Y)

cor(X, Y)




