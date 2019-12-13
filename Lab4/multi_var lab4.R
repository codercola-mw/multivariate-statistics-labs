aaa <- read.table('/Users/darin/Desktop/multi-var/P10-16.DAT')
aaa <- as.matrix(aaa)

s11 <- aaa[1:3,1:3]
s12 <- aaa[1:3,4:5]
s22 <- aaa[4:5,4:5]
s21 <- aaa[4:5,1:3]

#### produce the eigenvalues and eigenvectorsof s11####
eigen_x <- eigen(s11)
eigen_x

#### produce the eigenvalues and eigenvectorsof s22####
eigen_y <- eigen(s22)
eigen_y

####  diagonalization####
sqrex <- (eigen_x$vectors)%*%diag(eigen_x$values^-.5)%*%solve(eigen_x$vectors) 
sqrey <- (eigen_y$vectors)%*%diag(eigen_y$values^-.5)%*%solve(eigen_y$vectors)

####  estimate the eigenvalues and eigenvectors of sqrex and sqrey####
m1 <- sqrex%*%s12%*%solve(s22)%*%t(s12)%*%sqrex
m2 <- sqrey%*%t(s12)%*%solve(s11)%*%(s12)%*%sqrey
m1e <- eigen(m1)
m2e <- eigen(m2)

m1e
m2e

#### get a and b####
a <- sqrex%*%m1e$vectors
b <- sqrey%*%m2e$vectors

a
b

sqrt(0.26764579)
sqrt(0.01575231)

# We find two non-zero eigenvectors:
# The highest eigenvalue is 0.268, that is the canonical correlation is 0.518.
# The second eigenvalue is 0.016, that is the canonical correlation is 0.126.


# use the first eigenvectors a as weights for the first canonical correlation:
# raw, no standardize

# V_x_1 <- 0.013 * x1 - 0.014 * x2 + 0.023 * x3
# V_y_1 <- -8.066 * y1 + 0.019 * y2
# 
# the second canonical correlation:
# 
# V_x_2 <- 0.025 * x1 - 0.009 * x2 - 0.009 * x3
# V_y_2 <- 0.375 * y1 - 0.120 * y2

#### Significance test####
