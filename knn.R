# K-neigbours classification
# install.packages("ISLR")
library(ISLR)
library(class)
data(Default)
head(Default)
dim(Default)
# knn() requires that all predictors be numeric, 
# so we coerce student to be a 0 and 1 dummy variable instead of a factor. 
# (We can, and should, leave the response as a factor.) 
# Numeric predictors are required because of the distance calculations taking place.

set.seed(42)
Default$student = as.numeric(Default$student) - 1
default_idx = sample(nrow(Default), 5000)
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]

# training data
X_default_trn = default_trn[, -1]
y_default_trn = default_trn$default

# testing data
X_default_tst = default_tst[, -1]
y_default_tst = default_tst$default

# Training for k-nearest neighbors only is to simply remember the inputs. 
# Because of this, k-nearest neighbors is fast at training time. 
# However, at test time, k-nearest neighbors is very slow.
# For each test observation, the method must find the k-nearest neighbors, 
# which is not computationally cheap. 
# By deafult, knn() uses Euclidean distance to determine neighbors.

head(knn(train = X_default_trn, 
         test  = X_default_tst, 
         cl    = y_default_trn, 
         k     = 3))

# Here, the knn() function directly returns classifications. 
# That is knn() is essentially  $\hat{C}_k(x)$.

# Here, `knn()` takes four arguments:
  
# - `train`, the predictors for the train set.
# - `test`, the predictors for the test set. `knn()` will output results (classifications) for these cases.
# - `cl`, the true class labels for the train set.
# - `k`, the number of neighbors to consider.

# We’ll use () function to asses how well knn() 
# works with this data. We use the test data to evaluate.

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(actual    = y_default_tst,
               predicted = knn(train = X_default_trn,
                               test  = X_default_tst,
                               cl    = y_default_trn,
                               k     = 5))

# Often with `knn()` we need to consider the scale of the predictors variables. 
# If one variable is contains much larger numbers because of the units or 
# range of the variable, it will dominate other variables in the distance 
# measurements. But this doesn't necessarily mean that it should be such an 
# important variable. It is common practice to scale the predictors to have 
# a mean of zero and unit variance. 
# Be sure to apply the scaling to both the train and test data.

calc_class_err(actual    = y_default_tst,
               predicted = knn(train = scale(X_default_trn), 
                               test  = scale(X_default_tst), 
                               cl    = y_default_trn, 
                               k     = 5))

# How to choose k? 
# Try different values and see which works best.

set.seed(42)
k_to_try = 1:100
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = knn(train = scale(X_default_trn), 
             test  = scale(X_default_tst), 
             cl    = y_default_trn, 
             k     = k_to_try[i])
  err_k[i] = calc_class_err(y_default_tst, pred)
}

# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(y_default_tst == "Yes"), col = "grey", lty = 2)
dev.print(pdf, "kPlot.pdf")

min(err_k)

# One of k which is 24 are tied for the lowest error rate.
which(err_k == min(err_k))

# Run knn one more time with 24-neigbours
predicted = knn(train = scale(X_default_trn), 
                test  = scale(X_default_tst), 
                cl    = y_default_trn, 
                k     = 24)
# Table for prediction and actual test y values
table(predicted)
table(y_default_tst)

##create confusion matrix
tab <- table(predicted,y_default_tst)
tab
# The following function divides the correct predictions 
#by total number of predictions that tell us how accurate the model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# Reference: https://daviddalpiaz.github.io/r4sl/knn-class.html
