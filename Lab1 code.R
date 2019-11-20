
#importing data
data <- read.delim(file="~/Desktop/Lab1/T1-9.dat", sep="\t", header=FALSE)


#importing data
data <- read.delim(file="~/Desktop/Lab1/T1-9.dat", sep="\t", header=FALSE)

#Q1.a
charac <- data.frame(variable = as.numeric(), mean=as.numeric(), std_dev=as.numeric(), stringsAsFactors = FALSE)
for (i in 2:length(data)) {
  mean_v <- mean(data[[i]])
  std_dev_v <- sd(data[[i]])
  charac <- rbind(charac, data.frame(variable = colnames(data)[i], mean=mean_v, std_dev=std_dev_v))
}
charac

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

cov_mat<-cov(data[,-1])
cor_mat<-cor(data[,-1])
#### they are symmetric matrices

##Q2.B

pairs(data[,-1],pch=19,upper.panel = NULL) 
#It seems to outliers in each plot. Outlier tests can be necessary to class those observations as outliers


##Q2.3
#Chernoff faces is used here
#install.packages("aplpack")
library(aplpack)
faces(data[,-1],labels = data[,1])

## Q3.a
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
for (i in 1:7) {
  colnames(mean_correct_mat)[i] <- colnames(data)[i+1]
}
country <- as.vector(data[,1])
rownames(mean_correct_mat) <- country



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
Eulidean_dis <- matrix(dis)
row.names(Eulidean_dis) <- data[,1]
extreme_dis <- Eulidean_dis[order(Eulidean_dis, decreasing = TRUE)[1:5],]
print(extreme_dis)

eulidean_extreme<-sort(dis, decreasing = TRUE)[1:5]


##Q3 c.
v_1 <- as.vector(diag(cov_mat))
v_1 <- diag(v_1)
v_1 <- solve(v_1)
scale_dist <- matrix(0, nrow=nrow(mean_correct_mat))
rownames(scale_dist) <- country
colnames(scale_dist) <- "Scaled Distance"
for (i in 1:nrow(mean_correct_mat)) {
  scale_dist[i] <- t(as.matrix(mean_correct_mat[i,])) %*% v_1 %*% (as.matrix(mean_correct_mat[i,]))
}

sort(scale_dist, decreasing=TRUE)[1:5]
scale_dist[which(scale_dist > 11.3), ]

scale_extreme <- scale_dist[order(scale_dist, decreasing = TRUE)[1:5],]

#3.D
mahalanobis(data[,-1],mean_vect,cov_mat)

#Solution by matrix operations
d2values<-vector()
for(i in 1:nrow(data)){
  d2values[i]<-t(mean_correct_mat[i,])%*%solve(cov_mat)%*%mean_correct_mat[i,]
}
d2values
d2frame<-data.frame("Country"=data$Country,d2values)
d2frame<-d2frame[order(d2values,decreasing = TRUE),]
head(d2frame)

mat_2 <-  matrix(d2values)
row.names(mat_2) <- data[,1]
d2_extreme <- mat_2[order(mat_2, decreasing = TRUE)[1:5],]

# 3.e
library(RMaCzek)
eulidean_swe <- c(extreme_dis,Eulidean_dis["SWE",])
eulidean_com <- czek_matrix(eulidean_swe)
plot.czek_matrix(eulidean_com)

scale_swe <- c(scale_extreme, scale_dist["SWE",])
names(scale_swe)[6] <- "SWE"
scale_com <- czek_matrix(scale_swe)
plot.czek_matrix(scale_com)

d2_swe <- c(d2_extreme, mat_2["SWE",])
d2_com <- czek_matrix(d2_swe)
plot.czek_matrix(d2_com)

