#rm(list=ls())
library(boot)
require(stats)


##WRONG CV

# GENERATE DATA; use a seed for reproducibility
set.seed(4268)
n = 50  #number of observations
p = 5000  #number of predictors
d = 25  #top correlated predictors chosen

# generating predictor data
xs = matrix(rnorm(n * p, 0, 4), ncol = p, nrow = n)  #simple way to to uncorrelated predictors
dim(xs)  # n times p
# generate class labels independent of predictors - so if all
# classifies as class 1 we expect 50% errors in general
ys = c(rep(0, n/2), rep(1, n/2))  #now really 50% of each
table(ys)


corrs = apply(xs, 2, cor, y = ys) #Applies correlation formula between columns in xs and ys
hist(corrs)
selected = order(corrs^2, decreasing = TRUE)[1:d]  #top d correlated selected
data = data.frame(ys, xs[, selected])
logfit = glm(ys ~ ., family = "binomial", data = data)
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)
kfold = 10
cvres = cv.glm(data = data, cost = cost, glmfit = logfit, K = kfold)
cvres$delta


##Correct CV
reorder = sample(1:n, replace = FALSE)
validclass = NULL
for (i in 1:kfold) {
  neach = n/kfold
  trainids = setdiff(1:n, (((i - 1) * neach + 1):(i * neach)))
  traindata = data.frame(xs[reorder[trainids], ], ys[reorder[trainids]])
  validdata = data.frame(xs[reorder[-trainids], ], ys[reorder[-trainids]])
  colnames(traindata) = colnames(validdata) = c(paste("X", 1:p), "y")
  foldcorrs = apply(traindata[, 1:p], 2, cor, y = traindata[, p + 1])
  selected = order(foldcorrs^2, decreasing = TRUE)[1:d]  #top d correlated selected
  data = traindata[, c(selected, p + 1)]
  trainlogfit = glm(y ~ ., family = "binomial", data = data)
  pred = plogis(predict.glm(trainlogfit, newdata = validdata[, selected]))
  validclass = c(validclass, ifelse(pred > 0.5, 1, 0))
}
table(ys[reorder], validclass)
1 - sum(diag(table(ys[reorder], validclass)))/n


##Bootstrap
library(car)
library(boot)
SLID = na.omit(SLID)
n = dim(SLID)[1]
SLID.lm = lm(wages ~ ., data = SLID)
summary(SLID.lm)$coeff["age", ]
plot(SLID,col=SLID$sex)

library(carData)
boot.fn = function(data, index) {
  return(coef(lm(wages ~ ., data = SLID, subset = index)))
}

beta_hat = c()
B = 1000
for (i in 1:B) {
  beta_hat[i] = boot.fn(SLID, sample(nrow(SLID), nrow(SLID), replace = T))["age"]
}
library(ggplot2)
data = data.frame(beta_hat = beta_hat, norm_den = dnorm(beta_hat, mean(beta_hat), sd(beta_hat)))
ggplot(data) + geom_histogram(aes(x = beta_hat, y = ..density..), fill = "grey80", 
                              color = "black") + geom_line(aes(x = beta_hat, y = norm_den), color = "red") + theme_minimal()



