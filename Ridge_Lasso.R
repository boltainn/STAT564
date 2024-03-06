#Ridge and lasso

#install.packages("glmnet")
library("glmnet")
library("ISLR")

?Hitters
names(Hitters)
dim(Hitters)
head(Hitters)


sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

summary(Hitters)

x=model.matrix(Salary ~ .,Hitters)[,-1]

y=Hitters$Salary

#Ridge regression

grid <- 10^seq(10,-2,length=100)
grid

ridge.mod = glmnet(x,y,alpha=0, lambda = grid)
coef(ridge.mod)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

ridge.mod$lambda[60]
coef(ridge.mod)[,60]

predict(ridge.mod,s=50, type="coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
train
y.train <- y[train]
y.test <- y[-train]


ridge.mod=glmnet(x[train,], y[train], alpha = 0, lambda = grid,thresh = 1e-12)

ridge.pred <- predict(ridge.mod, s=4, newx = x[-train,])
mean((ridge.pred-y.test)^2)

ridge.pred <- predict(ridge.mod, s=10, newx = x[-train,])
mean((ridge.pred-y.test)^2)

set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train], alpha=0)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx = x[-train,])
mean((ridge.pred-y.test)^2)
