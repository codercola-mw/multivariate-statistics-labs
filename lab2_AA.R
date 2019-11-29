#LAB 2 Mult.Stat 732A97
my_data <- read.delim(file="C:\\Users\\Suat\\Desktop\\Master_courses\\732A97_Multivariate\\Materials\\T1-9.dat", sep="\t", header=FALSE)
colnames(my_data)<-c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")



####################--------Question 1-------#####################
#install.packages("outliers")
#install.packages("mvoutlier")
#install.packages("ellipse")
library(outliers)
library(mvoutlier)
library(ellipse)
########-------A)

data<-my_data #This row is necessary since I will use some rows from previous lab where the data was named "data".
 
charac <- data.frame(variable = as.numeric(), mean=as.numeric(), std_dev=as.numeric(), stringsAsFactors = FALSE)
for (i in 2:length(data)) {
  mean_v <- mean(data[[i]])
  std_dev_v <- sd(data[[i]])
  charac <- rbind(charac, data.frame(variable = colnames(data)[i], mean=mean_v, std_dev=std_dev_v))
}
mean_vect <- as.vector(charac[,2])
data_mat <- as.matrix(data[,-1])
mean_mat <- matrix(0, ncol=7, nrow=nrow(data_mat))
for (i in 1:nrow(data_mat)) {
  mean_mat[i,] <- mean_vect
}
mean_correct_mat <- data_mat - mean_mat
cov_mat<-cov(data[,-1])
cor_mat<-cor(data[,-1])
d2values<-vector()
for(i in 1:nrow(data)){
  d2values[i]<-t(mean_correct_mat[i,])%*%solve(cov_mat)%*%mean_correct_mat[i,]
}
d2values

#Outlier detection using mahalanobis distance
frame_mah<-data.frame("Country"=my_data$Country,d2values)
frame_mah$pvalues<-1-pchisq(frame_mah$d2values,df=2)
frame_mah[frame_mah$pvalues<0.01,] #Outliers

#Adjusted for a multiple-testing correction using bonferroni correction.
#Multiple testing correction adjusts the individual p-value for each
#gene to keep the overall error rate to less than or equal to the user-specified
#p-cutoff value. 
#Bonferroni correction takes each value and multiply by the number of observations. If corrected
#p-value is still below cutoff than the observation is significant.

frame_mah$adj_pvalues<-p.adjust(frame_mah$pvalues,method = "bonferroni")
frame_mah[frame_mah$adj_pvalues<0.01,] #Outliers. Adjusted p-value result

########-------B)
#Euclidean distance assumes data to be Gaussian and treat each feature equally. Mah.Dist. measure
#the correlation between variable and this can lead to different conclusions about the features.


####################--------Question 2-------#####################
#Load data

bird_data<-read.table("C:\\Users\\Suat\\Desktop\\Master_courses\\732A97_Multivariate\\Materials\\T5-12.DAT",header = FALSE)
colnames(bird_data)<-c("tail","wing")

########-------A)
the_means<-apply(bird_data,2,mean)
one_matrix<-matrix(1,nrow=dim(bird_data)[1],ncol = 1)

#Cov matrix
M_mean<-matrix(data=1,nrow=nrow(bird_data))%*%cbind(mean(bird_data$tail),mean(bird_data$wing))
diff_matrix<-as.matrix(bird_data-M_mean)
covs<-((nrow(bird_data)-1)^-1)*t(diff_matrix)%*%diff_matrix

#Confidence interval
mu1<-190
mu2<-275
n<-nrow(bird_data)
tmp_vector<-c(the_means[1]-mu1,the_means[2]-mu2)
n*t(tmp_vector)%*%solve(covs)%*%tmp_vector

2*(n-1)/(n-2)*qf(0.95,ncol(bird_data),n-2) #The F-value from page 222

#Since the Test-statistic is lower than the critical F-value the new mean values is in the confidence region and is therefore plausible.
#install.packages("ellipse")
library(ellipse)

#Plot the ellipse. Should be done without the R-function?
tmp<-ellipse(covs/n,centre = the_means,level = 0.95)
plot(tmp,type = "l")
points(x=190,y=275,col="red") #Same conclusion as before, the dot is in the confidence region and the H0 can not be rejected.

########-------B)
#T2 confidence interval. Formula from Page 2.25
mu1_vector<-c(1,0)
the_means
covs
n_col<-ncol(bird_data)
n<-nrow(bird_data)
crit_value<-sqrt((n_col*(n-1))*qf(0.95,n_col,n-n_col)/(n-n_col))
t2_res<-matrix(NA,ncol = 2,nrow = 2);colnames(t2_res)<-c("Lower","Upper");rownames(t2_res)<-c("Tail","Wing")
#mu1
t2_res[1,1]<-t(mu1_vector)%*%the_means-crit_value*sqrt((t(mu1_vector)%*%covs%*%mu1_vector)/n)
t2_res[1,2]<-t(mu1_vector)%*%the_means+crit_value*sqrt((t(mu1_vector)%*%covs%*%mu1_vector)/n)
#mu2
mu2_vector<-c(0,1)
t2_res[2,1]<-t(mu2_vector)%*%the_means-crit_value*sqrt((t(mu2_vector)%*%covs%*%mu2_vector)/n)
t2_res[2,2]<-t(mu2_vector)%*%the_means+crit_value*sqrt((t(mu2_vector)%*%covs%*%mu2_vector)/n)
t2_res


#Bonferroni. Formula from page 234. Critical value is the only difference from the formula above.
bon_res<--matrix(NA,ncol = 2,nrow = 2);colnames(bon_res)<-c("Lower","Upper");rownames(bon_res)<-c("Tail","Wing")
crit_value_bon<-qt(1-(0.05/(2*n_col)),n-1)

bon_res[1,1]<-t(mu1_vector)%*%the_means-crit_value_bon*sqrt((t(mu1_vector)%*%covs%*%mu1_vector)/n)
bon_res[1,2]<-t(mu1_vector)%*%the_means+crit_value_bon*sqrt((t(mu1_vector)%*%covs%*%mu1_vector)/n)
#mu2
mu2_vector<-c(0,1)
bon_res[2,1]<-t(mu2_vector)%*%the_means-crit_value_bon*sqrt((t(mu2_vector)%*%covs%*%mu2_vector)/n)
bon_res[2,2]<-t(mu2_vector)%*%the_means+crit_value_bon*sqrt((t(mu2_vector)%*%covs%*%mu2_vector)/n)
bon_res

#T2 intervals are larger than Bonferroni.
#T2 intervals takes the correlation between measured variables into account.
#If only interested in the component means, the Bonferonni intervals provide more precise estimates.
#The difference does not depent on mean vector or covariance matrix. It depends on the critical value which obtains the length of the invervals.

########-------C)
par(mfrow=c(1,3),pch=1) 
qqnorm(bird_data$tail,main = "Normal Q-Q Plot Tail Length")
qqnorm(bird_data$wing,main = "Normal Q-Q Plot Wing Length")
plot(bird_data$tail,bird_data$wing,xlab = "Tail",ylab="Wing",main="Scatterplot Tail vs Wing")
#From the plots, normality assumption seems not to be viable in this case.

####################--------Question 3-------#####################
#install.packages("heplots")
library(heplots)
library(corrplot)

skulls<-Skulls
########-------A)

#Boxplots for each variable
par(mfrow=c(2,2))
boxplot(skulls$mb~skulls$epoch,xlab = "Epoch",ylab="MB",main="MB")
boxplot(skulls$bh~skulls$epoch,xlab = "Epoch",ylab="BH",main="BH")
boxplot(skulls$bl~skulls$epoch,xlab = "Epoch",ylab="BL",main="BL")
boxplot(skulls$nh~skulls$epoch,xlab = "Epoch",ylab="NH",main="NH")

#Matrix scatterplot
pairs(skulls[,-1],pch=19,upper.panel = NULL) #No patterns can be detected. Uncorrelated variables.

#Correlationplot
corrplot(cor(skulls[,-1])) #Low correlations between the variables


########-------B)
#install.packages("MVTests")
library(MVTests)
tmp2<-Manova(data=skulls[,-1],group = skulls[,1])
summary(tmp2) #P-value almost. Reject H0. There are differences between the mean vectors.

########-------3)

