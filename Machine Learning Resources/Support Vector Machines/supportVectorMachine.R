### Support Vector Machine ###
library(ggplot2)
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
summary(tune.out)

# Lets predict the test set
y.pred <- predict(tune.out$best.model, dat[-train.index,])
dim(dat[-train.index,])

# Lets see the accuracy of the model with the lowest error
table(true=dat[-train.index,"y"], pred=predict(tune.out$best.model, dat[-train.index ,]))

# Now lets plot the ROC Curves
install.packages("ROCR")
library(ROCR)
rocplot <- function(pred,truth,...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)
}
svm.fit.opt <- svm(y~., data=dat[train.index,], kernel="radial", gamma = 2, cost = 1, decision.values = TRUE)
fitted <- attributes(predict(svm.fit.opt, dat[train.index,], decision.values=TRUE))$decision.values
attributes(predict(svm.fit.opt, dat[train.index,], decision.values=TRUE))
rocplot(fitted, dat[train.index,"y"], main = "Training Data")

