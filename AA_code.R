
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
pairs(data[,-1],pch=19,upper.panel = NULL) 
#It seems to be outliers in every plot. 


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

class(mean_correct_mean)



###Part of 3.E. To run this you need the codes from 3.B,3.C and 3.D in same script
###Plot. Comparing distances between diffent distance measurement methods
#Sweden is not extreme but have been included in the plot anyway
#The square distance and mahalabonis distance is almost same for SWE and thats why the red point for Sweden is "missing"

swe_frame<-data.frame(rep("SWE",3),c(Eulidean_dis["SWE",][[1]],scale_dist["SWE",],d2frame["49",2]),
                      c("sq_dist","extreme_dis","mahal_dis"))
colnames(swe_frame)<-colnames(distance_frame)        

sq_dist_frame<-data.frame("Country"=names(sq_dist),"distance"=sq_dist);rownames(sq_dist_frame)<-1:nrow(sq_dist_frame)
extreme_dis_frame<-data.frame("Country"=names(extreme_dis),"distance"=extreme_dis);rownames(extreme_dis_frame)<-1:nrow(sq_dist_frame)
mah_frame<-as.data.frame(d2frame[1:5,]);rownames(mah_frame)<-1:5;colnames(mah_frame)<-c("Country","distance")
dist_names<-c(rep("sq_dist",5),rep("extreme_dis",5),rep("mahal_dis",5))

distance_frame<-data.frame(rbind(sq_dist_frame,extreme_dis_frame,mah_frame),dist_names)
distance_frame<-rbind(distance_frame,swe_frame)

ggplot(distance_frame,aes(x=Country,y=distance,color=dist_names))+geom_point(size=3)+theme_bw()+
  theme(legend.title = element_blank())+ylab("Distance")

