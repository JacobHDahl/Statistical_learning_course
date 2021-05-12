#rm(list=ls())
library(matlib)

##Linear discriminant analysis

#Merged covariance matrix from both real and fake classes
cov <- matrix(nrow=2,ncol=2)
cov[1,1]=0.1371
cov[2,1]=0.00855
cov[1,2]=0.00855
cov[2,2]=0.2555

x<-c(214.0,140.4)
mu_k<-c(214.97,141.52) #real mean
pred <- x%*%inv(cov)%*%mu_k - 0.5*t(mu_k)%*%inv(cov)%*%mu_k + log(0.5)
mu_k2 <-c(214.82,139.45) #fake mean
pred2 <- x%*%inv(cov)%*%mu_k2 - 0.5*t(mu_k2)%*%inv(cov)%*%mu_k2 + log(0.5)

#Eq. 4.19 in book
#We run predictions with both means, the higher one gets the classification
#pred returns 198677.1 and pred2 returns 198668.3, meaning the fake one is higher
#-->we classify as FAKE


##Quadratic discriminant analysis

#Same procedure as LDA, but different formula
#Eq. 4.23 in book

covMatrix.real <- matrix(c(0.1502, 0.0055, 0.0055, 0.1998), byrow = T, ncol = 2) #magic matrix-making function
covMatrix.fake <- matrix(c(0.124, 0.0116, 0.0116, 0.3112), byrow = T, ncol = 2)
mean.real <-c(214.97,141.52)
mean.fake <- c(214.82,139.45)

x<-c(214.0,140.4) #input measurements to be classified

bayesClass.real <- -0.5*t(x)%*%inv(covMatrix.real)%*%x + t(x)%*%inv(covMatrix.real)%*%mean.real - 
                    0.5*t(mean.real)%*%inv(covMatrix.real)%*%mean.real - 0.5*log(det(covMatrix.real)) + log(0.5)


bayesClass.fake <- -0.5*t(x)%*%inv(covMatrix.fake)%*%x + t(x)%*%inv(covMatrix.fake)%*%mean.fake - 
  0.5*t(mean.fake)%*%inv(covMatrix.fake)%*%mean.fake - 0.5*log(det(covMatrix.fake)) + log(0.5)

#fake is higher --> FAKE


##Logistic regression task 4

x1 <- 40 #hours studied
x2 <- 3.5 #undergrad GPA

#regression coefficients
betaHat.0 <- -6
betaHat.1 <- 0.05
betaHat.2 <- 1 

pred <- exp(betaHat.0 + betaHat.1*x1 + betaHat.2*x2)/(1+exp(betaHat.0 + betaHat.1*x1 + betaHat.2*x2))
#return 0.377, which is the probability of this student getting an A

#How many hours are needed to get an A with the 3.5 GPA?

x1.est <- (log(0.5/(1-0.5)) - betaHat.0 - betaHat.2*x2)/betaHat.1
#Same formula, but solve for x1
#returns 50 hours

##Data analysis with R

#Sensitivity is the percentage of correct POSITIVES which are identified
#Specificity is the percentage of correct NEGATIVES which are identified

require(stats)
require(MASS)
require(class)
require(pROC)

data("Weekly")
summary(Weekly)
pairs(Weekly,col=Weekly$Direction) #plots all covariates against each other with the binary direction as color

