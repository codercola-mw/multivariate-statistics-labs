
#importing data
data <- read.delim(file="C:\\Users\\Suat\\Desktop\\Master_courses\\732A97_Multivariate\\Materials\\T1-9.dat", sep="\t", header=FALSE)
colnames(data)<-c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")

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
#cov_mat <- t(mean_correct_mat) %*% mean_correct_mat

cov_mat<-cov(data[,-1])
cor_mat<-cor(data[,-1])

##Q2.B
pairs(data[,-1],pch=19,upper.panel = NULL) #It seems to outliers in each plot. Outlier tests can be necessary to class those observations as outliers


##Q2.C
#Chernoff faces is used here
#install.packages("aplpack")
library(aplpack)
faces(data[,-1],labels = data[,1])

#Pairwise correlation between the variables. Plot.
#install.packages("corrplot")
library(corrplot)
corrplot(cor_mat) #Very high correlations between all the variables


#Multivariate residuals plot. Outliers can be detected from the plots here
mean_correct_mat2<-as.data.frame(mean_correct_mat)
mean_correct_mat2$country<-data$Country
rownames(mean_correct_mat2)<-data[,1]
colnames(mean_correct_mat2)<-c(colnames(data[,-1]),"country")

#With Loop. Use this one!
par(mfrow=c(3,3))
for(i in 1:ncol(mean_correct_mat2[,-8])){
 a<-plot(x=1:nrow(mean_correct_mat2),y=mean_correct_mat2[,i],
         xlab = "Country",ylab=colnames(mean_correct_mat2)[i])
  text(1:nrow(mean_correct_mat2), mean_correct_mat2[,i], labels=mean_correct_mat2$country, cex= 0.7, pos=1)
  print(a)
}



#Q.3.D

#Solution given by the in-built R-function "mahalanobis"
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
