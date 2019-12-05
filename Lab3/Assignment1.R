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
  data[,i] <- ((data[,i]-mean(data[,i]))/var(data[,i]))
}
SIG <- cov(data)
eigenvalSIG <- eigen(SIG)$values
eigenvectSIG <- eigen(SIG)$vectors


###. correlation table with names, cummlative percentage of sample variance by two components

cumvar <- c()
for (i in 1:2) {
  res <- eigenvalSIG[i]/sum(eigenvalSIG)
  cumvar <- append(cumvar, res)
}

cumvar <- cumsum(cumvar)
