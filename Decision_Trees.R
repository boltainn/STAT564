#install.packages("tree")
library(tree)
#install.packages("ISLR2")
library(ISLR2)

#Classification
head(Carseats)
?Carseats

attach(Carseats)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
High
HighCarseats <- data.frame(Carseats, High)
head(HighCarseats)

tree.carseats<-tree(High ~ .-Sales,Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty=0)

set.seed(2)
train <- sample(1:nrow(Carseats),200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ .-Sales, Carseats,
                      subset=train)

tree.pred <- predict(tree.carseats, Carseats.test, type ="class")

tree.pred
table(tree.pred, High.test)

set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")

plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree.carseats,best=9)

plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred,High.test)


#Bagging
#install.packages("randomForest")
library(randomForest)

head(Boston)
train <- sample(1:nrow(Boston)/2)
set.seed(1)
bag.boston <- randomForest(medv~., data=Boston,
                           subset=train, mtyr=12, importance=TRUE)
bag.boston
dim(Boston)
Boston.test=Boston$medv[-train]

yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag,Boston.test)


