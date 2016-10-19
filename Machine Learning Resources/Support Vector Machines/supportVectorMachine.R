### Support Vector Machine ###
library(ggplot2)
library(e1071)
set.seed (1)
x=matrix(rnorm(200*2), ncol=2)

# Make the first 150 samples encompass the last 50 samples, so that it is not linearly separable 
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

# Plot the data
plot(x, col = y)

# Or use ggplot which is nicer :O:O
p1 <- ggplot(dat, aes(x=x.1, y=x.2))
p2 <- p1 + geom_point(aes(color = y)) ; p2

# Now lets split our data into train and test
train.index <- sample(200, 100)

# Now we fit a model, noting here that we use a radial kernel 
svm.fit <- svm(y~., data = dat[train.index,], kernel = "radial", gamma = 1, cost = 1)
summary(svm.fit)

# We see here that there are a fair amount of training errors
plot(svm.fit, dat[train.index,])

# Lets try a model that has a higher cost => we try to fit the training data better => thinner margin => may overfit
svm.fit <- svm(y~., data = dat[train.index,], kernel ="radial", gamma = 1, cost = 1e5)

# We see here that there are practically no errors, but the decision boundary looks very weird, and is most likely overfitting
plot(svm.fit, dat[train.index,])

# Now as in support vector classifiers we can use the tune method in e1071 
set.seed(1)
tune.out=tune(svm, y~., data=dat[train.index,], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4) ))

# See here that best parameters are cost = 1 and gamma = 2
summary(tune.out)

# Lets predict the test set
y.pred <- predict(tune.out$best.model, dat[-train.index,])
dim(dat[-train.index,])

# Lets see the accuracy of the model with the lowest error
table(true=dat[-train.index,"y"], pred=predict(tune.out$best.model, dat[-train.index ,]))

# Need to expand on explaination of the ROC curves
# # Now lets plot the ROC Curves
# library(ROCR)
# rocplot <- function(pred,truth,...) {
#   predob <- prediction(pred, truth)
#   perf <- performance(predob, "tpr", "fpr")
#   plot(perf,...)
# }
# 
# # First fit the new and improved model
# svm.fit.opt <- svm(y~., data=dat[train.index,], kernel="radial", gamma = 2, cost = 1, decision.values = TRUE)
# 
# # Since we need the scores that are assigned to each 
# fitted <- attributes(predict(svm.fit.opt, dat[train.index,], decision.values=TRUE))$decision.values
# attributes(predict(svm.fit.opt, dat[train.index,], decision.values=TRUE))
# rocplot(fitted, dat[train.index,"y"], main = "Training Data")

# Lets see how overfitting can happen by tuning the gamma parameter

# See that our accuracy is 91/100 (on the training set), and 90/100 on the test set, although this is just on the training data, we could theoretically overfit and get a higher accuracy (on the training set) by increasing the gamma
table(true = dat[train.index, "y"], pred = predict(svm.fit.opt, dat[train.index,]))
table(true = dat[train.index, "y"], pred = predict(svm.fit.opt, dat[train.index,]))

# Lets use a high level of gamma and cost, remember as cost = C_{e1071} decreases it means that C_{ISL} increases, 
# which means that we have more "budget" to have wrong classifications that are wrong or on the wrong side of the margin, hence have a wider margin
# Hence to overfit we would want a high gamma, and also a high cost as this would mean that we would have a small value of total allowable error
svm.fit.overfit <- svm(y~., data = dat[train.index,], kernel = "radial", gamma = 100, cost = 100, decision.values = TRUE)

# See that our acc is 100% on the training set with gamma = 100, cost = 100, and 98% with gamma = 100, cost = 1
table(true = dat[train.index, "y"], pred = predict(svm.fit.overfit, dat[train.index,]))

# But our prediction on the test set suffers with 81% accuracy with gamma = 100, cost = 100
table(true = dat[-train.index, "y"], pred = predict(svm.fit.overfit, dat[-train.index,]))

