#rm(list=ls())
library(MASS)
library(ISLR)


##Single linear regression
plot(medv ~ lstat,Boston)
fit1 = lm(medv~lstat,data=Boston)
abline(fit1,col="red") #plots a line of fit1
confint(fit1) #confidence interval
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")

##Multiple linear regression
fit2 = lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3 = lm(medv~.,Boston) #fits a linear model(lm) for all (.) variables available against medv
summary(fit3)
par(mfrow=c(2,2)) #configures plot to 2x2 matrix of plots
plot(fit3)
fit4=update(fit3,~.-age-indus) #fit against all available -age and indus
summary(fit4)

##Nonlinear terms and interactions
fit5=lm(medv~lstat*age,Boston) #fits an linear model with an interaction AND the individual values (hierarchy principle)
summary(fit5)
fit6=lm(medv~lstat + I(lstat^2),Boston); summary(fit6) #adds a quadratic term, need I() to protect against the ^2 which is interprated differently in R (:)
attach(Boston) #makes the named variables in Boston available
par(mfrow=c(1,1)) #configures plot window with 1 plot
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20) #plots each value from lstat against the fitted values of fit6
fit7=lm(medv~poly(lstat,4)) #linear model with 4th power term
fit8=lm(medv~poly(lstat,2)) #better way of fitting polynomial than the I(^2) thingy
points(lstat,fitted(fit7),col="blue",pch=20)
points(lstat,fitted(fit8),col="green",pch=10)
#plot(1:20,1:20,pch=1:20,cex=2) #20 availabelplotting characters, cex=2 doubles size of character

##Qualitative predictors
fix(Carseats) #gives editor with table of data
names(Carseats) #gives names of carseats :)
summary(Carseats) #gives summary of carseats. Both quantitative(with a mean etc.) and categorical/qualitative(binary toggle parameters)
fit10=lm(Sales~.+Income:Advertising+Age:Price,Carseats) # :-operator specifies interaction between variables WITHOUT the variables themselves since they are included in the .-operator
summary(fit10)
contrasts(Carseats$ShelveLoc) #shows how R encodes qualitative variables when put in a linear model (Shows dummy variables)

##Writing R functions
regplot = function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){ #allows to pass in as many arguments as wanted since they will be used the same way inside the function
  fit=lm(y~x)
  plot(x,y,...) ##here it will use exactly those arguments which are included in the function call
  abline(fit,col="red")
}
regplot(Price, Sales, xlab="Price",ylab="Sales",col="blue", pch=20)
