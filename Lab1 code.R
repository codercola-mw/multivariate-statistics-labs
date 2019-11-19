
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


##Q1. b


##Q2. a
cov_mat<-cov(data[,-1])
cor_mat<-cor(data[,-1])
#### they are symmetric matrices

##Q2.B

pairs(data[,-1],pch=19,upper.panel = NULL) #It seems to outliers in each plot. Outlier tests can be necessary to class those observations as outliers

##Q2.3
#Chernoff faces is used here
install.packages("aplpack")
library(aplpack)
faces(data[,-1])












