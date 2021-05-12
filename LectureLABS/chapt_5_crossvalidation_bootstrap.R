require(ISLR)
require(boot)

plot(mpg~horsepower,data=Auto)

##Leave-one-out cross-validation (LOOCV)
glm.fit <- glm(mpg~horsepower,data=Auto)
summary(glm.fit)
cv.glm(Auto,glm.fit)$delta
