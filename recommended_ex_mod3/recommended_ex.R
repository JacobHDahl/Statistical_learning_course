#rm(list=ls())

##Setup
library(ISLR)
library(GGally)
library(ggfortify)
Auto = subset(Auto, select=-name)
summary(Auto)
str(Auto) #shows type of all variables ++
Auto$origin = factor(Auto$origin) #changes the origin variable to a qualitative variable. Needed to not fuck up
contrasts(Auto$origin) #this errors if the above change did not take

#ggpairs(Auto) #scary function which makes matrix of all variables against each other
AutoNoOrigin = subset(Auto,select=-origin) #removes origin and stores in new datasetVar
Cl <- cor(AutoNoOrigin)#computes correlation matrix
Cl <- cor(Auto[,-c(8)]) #minus column 8. Better way to omit origin without making a new variable
symnum(Cl) #bad visualization of corrmatrix

##Make linear model
fit1 = lm(mpg~.,data=Auto)
summary(fit1)
#Shows a relationship between miles pr gallon(mpg) and all other variables, except cylinders, horsepower and acceleration
#Negative slope for weight, meaning the lower weigth, the higher mpg which makes sense.
#The year coeff. suggests that higher year(younger) gives higher mpg, which also makes sense

anova(fit1) #for task d

set.seed(2332)
n = 100

par(mfrow = c(2, 3))
for (i in 1:6) {
  sim = rnorm(n)
  qqnorm(sim, pch = 1, frame = FALSE)
  qqline(sim, col = "blue", lwd = 1)
}

fit2 = lm(mpg~displacement+weight+year*origin, data=Auto)
summary(fit2)
anova(fit2)

Auto$sqrtmpg <- sqrt(Auto$mpg)

fit3 = lm(sqrtmpg ~ displacement + weight + year + origin, data = Auto)
autoplot(fit3)

fit4 = lm(mpg ~ displacement + I(log(weight)) + year + origin, data = Auto)
autoplot(fit4)

fit5 = lm(mpg ~ displacement + I(weight^2) + year + origin, data = Auto)
autoplot(fit5)


##Problem 2

# CI for beta_j
beta0 = 1
beta1 = 3
true_beta = c(beta0, beta1)  # vector of model coefficients
true_sd = 1  # choosing true sd
X = runif(100, 0, 1)
Xmat = model.matrix(~X, data = data.frame(X))  # Design Matrix


ci_int = ci_x = 0  # Counts how many times the true value is within the confidence interval
nsim = 1000
for (i in 1:nsim) {
  y = rnorm(n = 100, mean = Xmat %*% true_beta, sd = rep(true_sd, 100))
  mod = lm(y ~ x, data = data.frame(y = y, x = X))
  ci = confint(mod)
  ci_int[i] = ifelse(true_beta[1] >= ci[1, 1] && true_beta[1] <= ci[1, 2], 
                     1, 0)
  ci_x[i] = ifelse(true_beta[2] >= ci[2, 1] && true_beta[2] <= ci[2, 2], 1, 
                   0)
}

c(mean(ci_int), mean(ci_x))





# PI for Y_0
beta0 = 1
beta1 = 3
true_beta = c(beta0, beta1)  # vector of model coefficients
true_sd = 1  # choosing true sd
X = runif(100, 0, 1)
Xmat = model.matrix(~X, data = data.frame(X))  # Design Matrix

x0 = c(1, 0.4)

# simulating and fitting models many times
pi_y0 = 0
nsim = 1000
for (i in 1:nsim) {
  y = rnorm(n = 100, mean = Xmat %*% true_beta, sd = rep(true_sd, 100))
  mod = lm(y ~ x, data = data.frame(y = y, x = X))
  y0 = rnorm(n = 1, mean = x0 %*% true_beta, sd = true_sd)
  pi = predict(mod, newdata = data.frame(x = x0[2]), interval = "predict")[2:3]
  pi_y0[i] = ifelse(y0 >= pi[1] && y0 <= pi[2], 1, 0)
}

mean(pi_y0)
