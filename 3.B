
#importing data
data <- read.delim(file="~/Desktop/Lab1/T1-9.dat", sep="\t", header=FALSE)

#Q3.b
#COK, SAM, PNG, Korn, GBR, SIN

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
dis <- (diag(mean_correct_mat %*% t(mean_correct_mat))^(1/2))
Eulidean_dis <- data.frame(data[,1], Eulidean_dis)
extreme_dis <- country_dis[order(country_dis$Eulidean_dis, decreasing = TRUE)[1:5],]
print(extreme_dis)