
#importing data
data <- read.delim(file="~/Desktop/Lab1/T1-9.dat", sep="\t", header=FALSE)
colnames(data)<-c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")

##Q1. a
charac <- data.frame(variable = as.numeric(), mean=as.numeric(), std_dev=as.numeric(), stringsAsFactors = FALSE)
for (i in 2:length(data)) {
  mean_v <- mean(data[[i]])
  std_dev_v <- sd(data[[i]])
  charac <- rbind(charac, data.frame(variable = as.numeric(i-1), mean=mean_v, std_dev=std_dev_v))
}
charac


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

cov_mat<-cov(data[,-1])
cor_mat<-cor(data[,-1])
#### they are symmetric matrices

##Q2.B

pairs(data[,-1],pch=19,upper.panel = NULL) #It seems to outliers in each plot. Outlier tests can be necessary to class those observations as outliers




##Q2.3
#Chernoff faces is used here
install.packages("aplpack")
library(aplpack)
faces(data[,-1],labels = data[,1])


#COK, SAM, PNG, GUA

## Q3.a


mean_mat
pairs(mean_correct_mat, labels = data[,1])

text(mean_correct_mat, labels=data[,1])



