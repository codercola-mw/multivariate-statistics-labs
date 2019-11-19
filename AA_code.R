
#importing data
data <- read.delim(file="C:\\Users\\Suat\\Desktop\\Master_courses\\732A97_Multivariate\\Materials\\T1-9.dat", sep="\t", header=FALSE)
colnames(data)<-c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")

##Q1. a
charac <- data.frame(variable = as.numeric(), mean=as.numeric(), std_dev=as.numeric(), stringsAsFactors = FALSE)
for (i in 2:length(data)) {
  mean_v <- mean(data[[i]])
  std_dev_v <- sd(data[[i]])
  charac <- rbind(charac, data.frame(variable = as.numeric(i-1), mean=mean_v, std_dev=std_dev_v))
}
charac


##Q1.b


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
corrplot(cor_mat) #Very high correlations between the variables


#Multivariate residuals plot. Outliers can be detected from the plots here
mean_correct_mat<-as.data.frame(mean_correct_mat)
mean_correct_mat$country<-data$Country
rownames(mean_correct_mat)<-data[,1]
colnames(mean_correct_mat)<-c(colnames(data[,-1]),"country")

plot(x="country",y="100m",data=mean_correct_mat)

par(mfrow=c(3,3))
plot(x=1:54,y=mean_correct_mat$`100m`,xlab = "Country",ylab="100m")
text(1:54, mean_correct_mat$`100m`, labels=mean_correct_mat$country, cex= 0.7, pos=1)

plot(x=1:54,y=mean_correct_mat$`200m`,xlab = "Country",ylab="200m")
text(1:54, mean_correct_mat$`200m`, labels=mean_correct_mat$country, cex= 0.7, pos=1)

plot(x=1:54,y=mean_correct_mat$`400m`,xlab = "Country",ylab="400m")
text(1:54, mean_correct_mat$`400m`, labels=mean_correct_mat$country, cex= 0.7, pos=1)

plot(x=1:54,y=mean_correct_mat$`800m`,xlab = "Country",ylab="800m")
text(1:54, mean_correct_mat$`800m`, labels=mean_correct_mat$country, cex= 0.7, pos=1)

plot(x=1:54,y=mean_correct_mat$`1500m`,xlab = "Country",ylab="1500m")
text(1:54, mean_correct_mat$`1500m`, labels=mean_correct_mat$country, cex= 0.7, pos=1)

plot(x=1:54,y=mean_correct_mat$`3000m`,xlab = "Country",ylab="3000m")
text(1:54, mean_correct_mat$`3000m`, labels=mean_correct_mat$country, cex= 0.7, pos=1)

plot(x=1:54,y=mean_correct_mat$Marathon,xlab = "Country",ylab="Marathon")
text(1:54, mean_correct_mat$Marathon, labels=mean_correct_mat$country, cex= 0.7, pos=1)


#Q.3.B


