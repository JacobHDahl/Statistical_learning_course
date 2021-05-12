#rm(list=ls())
require(matlib)
require(tidyverse)
require(stats)
require(MASS)
require(class)
require(pROC)
require(pROC)
require(plotROC)


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


data("Weekly")
summary(Weekly)
pairs(Weekly,col=Weekly$Direction) #plots all covariates against each other with the binary direction as color

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family = "binomial")
summary(glm.fit)
#Lag2 gives p-value of 0.0296

#making confusion matrix
glm.probs_Weekly <- predict(glm.fit, type = "response")
glm.preds_Weekly <- ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
table(glm.preds_Weekly, Weekly$Direction)

Weekly_trainID <- (Weekly$Year < 2009)
Weekly_train <- Weekly[Weekly_trainID, ]
Weekly_test <- Weekly[!Weekly_trainID, ]

glm.fit1 <- glm(Direction~Lag2,data=Weekly_train,family = "binomial")
glm.probs_Weekly <- predict(glm.fit1,newdata=Weekly_test,type="response")
glm.preds_Weekly <- ifelse(glm.probs_Weekly>0.5,"Up","Down")
table(glm.preds_Weekly,Weekly_test$Direction)
summary(glm.fit1)


#LDA fit on same data
lda.fit<-lda(Direction~Lag2,data=Weekly_train)
lda.probs <- predict(lda.fit,newdata=Weekly_test,type="response")
table(lda.probs$class,Weekly_test$Direction)
mean(lda.probs$class==Weekly_test$Direction)

#QDA on same data
qda.fit <- qda(Direction~Lag2,data=Weekly_train)
qda.probs <- predict(qda.fit,newdata=Weekly_test,type="response")
table(qda.probs$class,Weekly_test$Direction)
mean(qda.probs$class==Weekly_test$Direction)

#KNN on same data
knn.train <- as.matrix(Weekly_train$Lag2)
knn.test <- as.matrix(Weekly_test$Lag2)

set.seed(123)
knn.fit <-knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, 
                   k = 12, prob = T)
table(knn.fit, Weekly_test$Direction)
mean(knn.fit==Weekly_test$Direction)

# knn error:
K <- 30
knn.error <- rep(NA, K)

set.seed(234)
for (k in 1:K) {
  knn.pred <- knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction, 
                 k = k)
  knn.error[k] <- mean(knn.pred != Weekly_test$Direction)
}
knn.error.df <- data.frame(k = 1:K, error = knn.error)
ggplot(knn.error.df, aes(x = k, y = error)) + geom_point(col = "blue") + geom_line(linetype = "dotted")

##Comparing methods using ROC

# get the probabilities for the classified class
knn.probs <- attributes(knn.fit)$prob

# since we want the probability for Up, we need to take 1-p for the elements
# that gives probability for Down
down <- which(knn.fit == "Down")
knn.probs[down] <- 1 - knn.probs[down]

glmroc <- roc(response = Weekly_test$Direction, predictor = glm.probs_Weekly,direction = "<")
ldaroc <- roc(response = Weekly_test$Direction, predictor = lda.probs$posterior[,1], direction = "<")
qdaroc <- roc(response = Weekly_test$Direction, predictor = qda.probs$posterior[,1], direction = "<")
knnroc <- roc(response = Weekly_test$Direction, predictor = knn.probs,direction = "<")

#Calculating area under curve of each roc
#Optimal predictor gived AUC=1, AUC=0.5 gives no information.
#Makes sense that these are close to no information since it is on stock market data
auc(glmroc)
auc(ldaroc)
auc(qdaroc)
auc(knnroc)

dat <- data.frame(Direction = Weekly_test$Direction, glm = glm.probs_Weekly, 
                 lda = lda.probs$posterior[,1], qda = qda.probs$posterior[,1], knn = knn.probs)
dat_long = melt_roc(dat, "Direction", c("glm", "lda", "qda", "knn"))
ggplot(dat_long, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = F) + xlab("1-Specificity") + 
  ylab("Sensitivity")
