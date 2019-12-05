# Importing data
data <- read.delim(file="C:/Users/Young/Documents/Multivariate/732A97_HT2019_Materials/T1-9.dat", sep="\t", header=FALSE)
colnames(data) = c("Country", "100m", "200m", "400m", "800m", "1500m", "3000m", "marathon")
data <- as.data.frame(data[,-1])


# Assignment 1

##a. Obtaining correlation matrix R, eigenvalues and eigenvectors
R <- cor(data)
eigen(R)
eigenval <- eigen(R)$values
eigenvect <- eigen(R)$vectors

##b. 
###. standardize the variable, get two principal components
for (i in 1:ncol(data)) {
  data[,i] <- ((data[,i]-mean(data[,i]))/sqrt(var(data[,i])))
}
SIG <- cov(data)
eigenvalSIG <- eigen(SIG)$values
eigenvectSIG <- eigen(SIG)$vectors


###. correlation table with names, cummlative percentage of sample variance by two components

Y1 <- matrix(0,nrow=nrow(data),ncol=1)
for (i in 1:7) {
  Y1 <- Y1+(eigenvect[,1][i]*data[,i])
}
a <- cor(data, Y1)

Y2 <- matrix(0,nrow=nrow(data),ncol=1)
for (i in 1:7) {
  Y2 <- Y2+(eigenvect[,2][i]*data[,i])
}
b <- cor(data, Y2)

RES <- matrix(c(a,b), ncol=2, nrow=7)
colnames(RES) <- c("PC1", "PC2")
rownames(RES) <- c("z1", "z2", "z3", "z4", "z5", "z6", "z7")



cumvar <- c()
for (i in 1:2) {
  res <- eigenvalSIG[i]/7
  cumvar <- append(cumvar, res)
}

cumvar <- matrix(cumsum(cumvar), nrow=1)
colnames(cumvar) <- c("PC1", "PC2")
rownames(cumvar) <- "Cummlative Explained Variance"
