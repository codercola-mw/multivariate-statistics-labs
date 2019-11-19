
#importing data
data <- read.delim(file="~/Desktop/Lab1/T1-9.dat", sep="\t", header=FALSE)

##Q1. a
charac <- data.frame(variable = as.numeric(), mean=as.numeric(), std_dev=as.numeric(), stringsAsFactors = FALSE)
for (i in 2:length(data)) {
  mean_v <- mean(data[[i]])
  std_dev_v <- sd(data[[i]])
  charac <- rbind(charac, data.frame(variable = as.numeric(i-1), mean=mean_v, std_dev=std_dev_v))
}
print(charac)


##Q1. b


##Q2. a
data_mat <- as.matrix(data[,-1])
for (i in 1:7) {
  colnames(data_mat)[i] <- paste("V", i, sep='')
}
mean_vect <- as.vector(charac[,2])
mean_mat <- matrix(0, ncol=7, nrow=nrow(data_mat))
for (i in 1:nrow(data_mat)) {
  mean_mat[i,] <- mean_vect
}
mean_correct_mat <- data_mat - mean_mat
cov_mat <- t(mean_correct_mat) %*% mean_correct_mat

##

##Q3. a
