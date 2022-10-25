########################################################
############### Basics of linear algebra in R ##########
########################################################
rm(list = ls())

# Defining a column vector:
a <- c(1,2,3)

# Although R prints the vector in row format, it is 
# considered as a column vector:
a

# Additional examples:
# Numbers from 4 to 6 using ":"
b <- c(4:6)
b

# Numbers drawn from a normal distribution:
d <- c(rnorm(10, m = 5, sd = 10))
d

# Multiplying a vector by a number:
a * 7

# Defining a matrix (by column):
A <- matrix(c(1,2,3,4,5,6), ncol = 3)
A

# Another example:
B <- matrix(c(1:6), ncol = 3)
B

# Defining a matrix (by row):
C <- matrix(c(1:100), ncol = 10, byrow = TRUE)
C

# Transposing a vector:
at <- t(a)

# Transposing a matrix:
At <- t(A)

# Sum of vectors (must have same length):
a + b

# Sum of matrices (must have same dimensions):
A + B

# Multiplying a matrix by a number:
2 * A

# Multiplication of a matrix and a vector
# Attention, they must be conformable:
# A is a 2x3 matrix and b is a 3x1 vector
# The product Ab is going to be a 2x1 vector
Ab <- A %*% b
Ab

# Multiplication of two matrices
# Matrix multiplication is not commutative
# Attention, the two matrices must be conformable! The number of columns of the first matrix
# must equal the number of rows of the second matrix
# A is a 2x3 matrix and B is a 3x2 matrix
# The product AB is a 2x2 matrix
D <- matrix(rnorm(6), ncol = 2, byrow = TRUE)
AB <- A %*% D
AB

# Some special matrices ----

# The null matrix:
NM <- matrix(0, nrow = 3, ncol = 3)
NM

# The null matrix works as ZERO in algebra
A %*% NM

# The identity matrix I
# One way to create the identity matrix
ID <- matrix(cbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)), ncol = 3)
ID

# Or we fill the diagonal of a null matrix with ONES
ID <- matrix(0, nrow = 3, ncol = 3)
diag(ID) <- 1
ID

# The identity matrix works as the number ONE in algebra
A %*% ID

# Inverting matrices ----
# The inverse of the matrix A (nxn) is the matrix A^-1 (nxn) which
# multiplied by A returns the identity matrix
# A*A^-1 = A^-1*A = I
M <- matrix(cbind(c(1,0,2), c(4,5,3), c(7,2,1)), ncol=3)
M
M_inv <- solve(M)

# Let's check (rounding the numbers)!
round(M %*% M_inv)
round(M_inv %*% M)

# Attention!Not all matrices are invertible!
# In order to be invertible, a matrix (also said non-singular) must meet several conditions:
# The columns are independent
# The rows are independent
# The matrix has full-rank (n = r)
# The determinant is not zero
# ...
N <- matrix(c(3,5,6,4,5,8,6,10,12), ncol = 3)
N

# solve(N)
# round(det(N))

# Problem of perfect multicollinearity in econometrics:
# Variables are multiple of each others

# Applications ----
# 1) Solving a system of two equations and two unknowns in matrix form:

# Let's load the package "matlib"
library(matlib)

A <- matrix(c(1, 3, -2, 2), 2, 2)
b <- c(1,11)
A
b

# Let's solve the model A*x = b using linear algebra 
# The solution vector "x" of the system above can be found as:
x <- solve(A) %*% b
x

# The ROW picture: ----
# Two lines meeting in the x-y plane: the intersection represents 
# the solution of the system
plotEqn(A,b)

# The COLUMN picture ----
# The combination of columns that produces the right-hand side
# 3*col1 + 1*col2 = b
plot(c(-3,3),c(0,11), xlab="x", ylab="y", pch=21, col="transparent", abline(h=0, v=0))
arrows(0,0,3,9, lwd=4,col="blue")
arrows(0,0,-2,2, lwd=4,col="red")
arrows(0,0,1,11, lwd=4,col="green")

# Not all system of equations have a solution
# Columns should not be a multiple of each other
B <- matrix(c(1, 3, -2, -6), 2, 2)
B 
c <- c(1,11)

# Matrix B is not invertible
#solve(B)

# The ROW picture
# Parallel lines do not meet
plotEqn(B,c)

# The COLUMN picture
# Columns don't combine to give the solution "c"
plot(c(-3,3),c(-6,3), xlab="x", ylab="y", pch=21, col="transparent", abline(h=0, v=0))
arrows(0,0,1,3,lwd=4,col="blue")
arrows(0,0,-2,-6, lwd=4, col="red")

# 2) Solving a system of three equations and three unknowns in matrix form:
A <- matrix(c(1,2,3,2,5,2,6,-3,1), 3, 3, byrow=TRUE)
b <- c(6,4,2)

# Let's solve the model A*x = b using again linear algebra 
x <- solve(A) %*% b
round(x)

# The ROW pictures shows three planes meeting at a single point
library(rgl)
plotEqn3d(A,b,x, xlim=c(-4,4), ylim=c(-4,4))

# The COLUMN picture shows a combination of vectors with weights (x,y,z) 0,0,2
# Loading the necessary package to perform 3D plotting
library(plot3D)

# Preparing the data to plot
# Starting point (read by column)
x0 <- c(0, 0, 0, 0)
y0 <- c(0, 0, 0, 0)
z0 <- c(0, 0, 0, 0)
# Ending point (read by column)
x1 <- c(1, 2, 3, 6)
y1 <- c(2, 5, 2, 4)
z1 <- c(6,-3, 1, 2)

# Plotting the vectors in the 3-dimensional space
arrows3D(x0, y0, z0, x1, y1, z1, phi = 20, theta = 50, col = c("red","blue","green","yellow"),
         lwd = 2, d = 3, bty = "f", colkey = FALSE, addlines = TRUE, length = 0.5, width = 0.5, ticktype = "detailed")
# Mark the origin in black
points3D(x0, y0, z0, add = TRUE, col="black", 
         colkey = FALSE, pch = 19, cex = 1)

