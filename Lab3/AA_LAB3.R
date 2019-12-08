#MULTIVARIATE LAB 2

############-----------QUESTION 1----------------##############

my_data <- read.delim(file="C:\\Users\\Suat\\Desktop\\Master_courses\\732A97_Multivariate\\Materials\\T1-9.dat", sep="\t", header=FALSE)
colnames(my_data)<-c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")

###### A.

#Correlation matrix
R_matrix<-cor(my_data[,-1])
#Eigenvalues and eigenvectors
eigen(R_matrix)

###### B.

#OBS! Here should a table showing cor between standardized variables with the components be created.

my_PCA<-prcomp(my_data[,-1],scale = TRUE)
summary(my_PCA) #PC1 and PC2 together explains 91.9 % percent of the variation

###### C.
#install.packages("factoextra")
library(factoextra)

fviz_contrib(my_PCA,choice = "var",axes = 1) #Contribution to PC1
#All variables contributes equally to dimension 1

fviz_contrib(my_PCA,choice = "var",axes = 2) #Contribution to PC2
#This component should be interpreted as IN THE QUESTION! 

#But from PC1 values from each variable, we can see that the short distance races have negative values

###### D.
scores_frame<-data.frame("Country"=my_data$Country,"Scores"=my_PCA$x[,1])
scores_frame<-scores_frame[order(scores_frame$Scores,decreasing = TRUE),]
head(scores_frame) #Seems reasonable


############-----------QUESTION 2----------------##############
         
###### USING FACTANAL() HERE.

##### Covariance matrix

S_matrix<-cov.wt(my_data[,-1])
R_matrix<-cor(my_data[,-1])

my_fact<-factanal(my_data[,-1],factors=2,covmat=S_matrix,n.obs=nrow(my_data))
cov_loads<-my_fact$loadings[,1:2]

plot(cov_loads);text(cov_loads,labels=names(my_data[,-1]))
#For factor 1: High values for long distance runs and low values for sprint runs.
#Factor 1 interpretas how good a country is good long distance runs.

#Factor 2: The interpretation of this factor is how good a nation is at sprint runs.


##### correlation matrix

my_fact_cor<-factanal(my_data[,-1],factors=2,covmat=R_matrix,n.obs=nrow(my_data))
cor_loads<-my_fact_cor$loadings[,1:2]
plot(cor_loads);text(cor_loads,labels=names(my_data[,-1]))

#Same result as the analysis with the covariance matrix.

#Outliers. Dont know how to specify Cov or Cor matrix here when calculating scores. These function runs when covmat="NULL"

my_fact3<-factanal(x=my_data[,-1],factors=2,scores="regression")

scores<-as.data.frame(my_fact3$scores[,c(1:2)])
scores$index<-1:54
plot(y=scores[,1],x=scores$index,col="red")
scores$index[which(scores[,1]>2)]
my_data[46,1] #Outlier from scores for factor 1. SAM


plot(y=scores[,2],x=scores$index,col="red")
scores$index[which(scores[,2]>1.5)]
my_data[c(11,31),1] #Outlier from scores for factor 2. COK and KORN

  
#### USING PC now. psych package. Rotation is by default "varimax"
install.packages("psych")
library(psych)

#Using correlation matrix
cor_fact<-principal(my_data[,-1],nfactors=2,method = "regression",n.obs = 54,scores=TRUE,oblique.scores = FALSE)
cor_fact$loadings[,1]

plot(cor_fact$loadings) #Same result as with factanal()

#Scores plot and outliers from that plot for FACTOR 1

scores<-as.data.frame(cor_fact$scores[,c(1:2)])
scores$index<-1:54
plot(y=scores[,1],x=scores$index,col="red")
scores$index[which(scores[,1]>2)]
my_data[c(40,46),1] #Outliers seems to be PNG and SAM

#Scores plot and outliers from that plot for FACTOR 2

scores<-as.data.frame(cor_fact$scores[,c(1:2)])
scores$index<-1:54
plot(y=scores[,2],x=scores$index,col="red")
scores$index[which(scores[,2]>2)]
my_data[c(11,31),1] #Outliers seems to be COK and KORN

#USING COVARIANCE MATRIX

cov_fact<-principal(my_data[,-1],nfactors=2,method = "regression",n.obs = 54,scores=TRUE,covar = TRUE)
cov_fact$loadings[,1]
plot(cov_fact$loadings) #For factor 1 Marathon load very highly on this factor. For factor 2 "800m" load higly.

#Scores plot for factor 1 

scores<-as.data.frame(cov_fact$scores[,c(1:2)])
scores$index<-1:54
plot(y=scores[,1],x=scores$index,col="red")
scores$index[which(scores[,1]>2.5)]
my_data[c(11,40),1] #Outliers seems to be COK and PNG

#Scores plot for factor 2

scores<-as.data.frame(cov_fact$scores[,c(1:2)])
scores$index<-1:54
plot(y=scores[,2],x=scores$index,col="red")
scores$index[which(scores[,1]>2)]
my_data[c(11,40,46),1] #Hard to pinpoint which countries are outliers here. But COK, PNG and SAM has high scores values.





