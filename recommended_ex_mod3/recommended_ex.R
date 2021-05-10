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
