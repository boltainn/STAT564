#Resampling
#LOOCV

library(ISLR)
library(Auto)
head(Auto)

glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta


cv.error <-sapply(1:5, function(i)
  cv.glm(Auto, glm(mpg ~ poly(horsepower, i), 
                   data = Auto))$delta[1])

cv.error <-sapply(1:10, function(i)
  cv.glm(Auto, glm(mpg ~ poly(horsepower, i), 
                   data = Auto))$delta[1])

pol <- 1:10
data <- cbind(pol, cv.error)

plot(data, ylim=c(15,28), type='b', pch=19, col="red", xlab="Degree of poly.", ylab="Mean Squared Error")

# K-fold CV
set.seed(17)
cv.error.ten <- cv.error <-sapply(1:10, function(i)
  cv.glm(Auto, glm(mpg ~ poly(horsepower, i), 
                   data = Auto),K=10)$delta[1])

# Bootstrap

alpha.fn <- function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=TRUE))

boot(Portfolio, alpha.fn, R=1000)

