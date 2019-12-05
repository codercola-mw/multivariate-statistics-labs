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
         
###### A.
S_matrix<-cov(my_data[,-1])
my_fact<-factanal(my_data[,-1],2,covmat = S_matrix)
#88.6 % of the variance is explanied by those 2 factors

factanal(S_matrix,2,covmat=S_matrix)

factanal(factor=2,covmat = S_matrix)

my_fact$correlation

