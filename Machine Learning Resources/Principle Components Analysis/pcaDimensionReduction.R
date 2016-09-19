### PCA for dimension reduction ###

# load iris data
data(iris)
str(iris)

# get rid of the last column as it is the response variable
dat <- as.matrix(iris[,-5])

# Use the out of the box pca function provided by R
pca <- prcomp(dat, retx=TRUE, center=TRUE, scale=TRUE)
summary(pca)

# Now do PCA by "hand" to get a better understanding, first scale the data by mean and variance, 
# although it doesn't really matter (?) if we scale variance or not just as long as we scale the mean

# So we need:
# 1. pca$rotation which are the eigenvectors of the scaled covariance matrix
# 2. pca$x which are the principle components

# 1. Get the eigenvectors of the scaled covariance matrix
dat.mean <- colSums(dat)/nrow(dat)
dat.scaled <- scale(dat, center = dat.mean)
dat.scaled.cov <- t(dat.scaled) %*% dat.scaled
u <- eigen(dat.scaled.cov)$vectors 

# see that these values are the same as pca$rotation
u
pca$rotation

# See what happens if we don't scale the data
dat.cov <- t(dat) %*% dat
eigen(dat.cov)

# 2. Get the principle components of the training data
pc <- dat.scaled %*% u

# See that the PC's are the same 
pca$x[1,]
pc[1,]


# So now the goal is to show how to use PC's for dimensionality reduction, first we will simulate 
# some new testing data
 
# Feature values are based on the distributions of the original data means
# and the covariances between these parameters. 

setosa.mean <- apply(iris[iris$Species=="setosa",-5], 2, mean)
setosa.cov <- cov(iris[iris$Species=="setosa",-5])
versicolor.mean <- apply(iris[iris$Species=="versicolor",-5], 2, mean)
versicolor.cov <- cov(iris[iris$Species=="versicolor",-5])
virginica.mean <- apply(iris[iris$Species=="virginica",-5], 2, mean)
virginica.cov <- cov(iris[iris$Species=="virginica",-5])

# Make new random data based on the calculated biometry info. each species
# The MASS package allows for the calculation of correlated/covarying random 
# numbers using this information.

require(MASS)
set.seed(1)
n <- 30
new.setosa <- mvrnorm(n, setosa.mean, setosa.cov)
new.versicolor <- mvrnorm(n, versicolor.mean, versicolor.cov)
new.virginica <- mvrnorm(n, virginica.mean, virginica.cov)

# Now to use PCA, we need to map the the data points on the original axis onto the new rotated axis

# Use the predict function that does the mapping for us (note here that the predict function simply maps
# the new.* data to the new coordinates as defined by the scaled covariance matrix of the initial training data)
pred.setosa <- data.frame(predict(pca, new.setosa))
pred.versicolor <- data.frame(predict(pca, new.versicolor))
pred.virginica <- data.frame(predict(pca, new.virginica))

# Now to do it by "hand"

# First center the data 
new.setosa.scale <- scale(new.setosa, scale = pca$scale, center=pca$center)

# Now that we have scaled the data we should project these points onto a new space by multiplying by the eigenvectors
# that we have previously found through prcomp


# We see that the first 3 PC's explain 99.5% of the variation, hence we map it from 4 dimensions to 3 dimensions
new.setosa.projected <- (new.setosa.scale %*% pca$rotation)

# We see that the values are the same 
new.setosa.projected[1,]
pred.setosa[1,]

# Recap on what we have done:
# 1. Used the pca function (and also did it by "hand") to find the eigenvectors needed for the change of basis of coordinates
# 2. Used the predict function (and also did it by "hand") of prcomp to map new testing data to the new coordinates

# Now we are finally read to see how we can use PCA for dimensionality reduction

# First lets use the iris data to train the LDA model
str(iris)
lda <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
lda$means

# Predict on the test data
pred.setosa <- predict(lda, data.frame(new.setosa))$class
pred.versicolor <- predict(lda, data.frame(new.versicolor))$class 
pred.virginica <- predict(lda, data.frame(new.virginica))$class

# see the accuracy
acc.setosa <- length(which(pred.setosa == "setosa"))/length(pred.setosa)
acc.versicolor <- length(which(pred.versicolor == "versicolor"))/length(pred.versicolor)
acc.virginica <- length(which(pred.virginica == "virginica"))/length(pred.virginica)

# Now lets create a LDA on the projected version of the iris data set
pca <- prcomp(dat, retx = TRUE, center = TRUE, scale = TRUE)
summary(pca)

# We see here that the first 2 components give us ~96% of the data's variation
pc.use = 2

# Now lets project the iris data onto our new axis, and create a non-truncated version and a truncated version
iris.projected <- data.frame(predict(pca, iris))
head(iris.projected)
iris.projected.truncated <- data.frame(predict(pca,iris)[,1:pc.use])
head(iris.projected.truncated)

# Add the species to it
iris.projected$Species <- iris$Species
iris.projected.truncated$Species <- iris$Species

# Now lets finally create LDA on the new projected space, note here that we only use the first 2 components
lda.projected <- lda(Species ~ PC1 + PC2, data = iris.projected)
lda.projected$mean

# Note how these two models are equivalent
lda.projected <- lda(Species ~ ., data = iris.projected.truncated)
lda.projected$mean

# Now lets see how the predictions fair, first we have to project our test data onto the new coordinates
setosa.projected <- data.frame(predict(pca, new.setosa))
versicolor.projected <- data.frame(predict(pca, new.versicolor))
virginica.projected <- data.frame(predict(pca, new.virginica))

# Now predict the class
pred.setosa.projected <- predict(lda.projected, setosa.projected)$class
pred.versicolor.projected <- predict(lda.projected, versicolor.projected)$class
pred.virginica.projected <- predict(lda.projected, virginica.projected)$class

# see the accuracy
acc.setosa <- length(which(pred.setosa.projected == "setosa"))/length(pred.setosa.projected) # accuracy of 1
acc.versicolor <- length(which(pred.versicolor.projected == "versicolor"))/length(pred.versicolor.projected) # accuracy of 0.766
acc.virginica <- length(which(pred.virginica.projected == "virginica"))/length(pred.virginica.projected) # accuracy of 0.9

# ------------------------------------------------------------------------------------ #

# Lets see what happens if we create an LDA model on all of the data
lda.projected.all <- lda(Species ~. ,data = iris.projected)

# predict class of our test data using all the principle component or in other words, not doing any dimensionality reduction
pred.setosa.projected.all <- predict(lda.projected.all, setosa.projected)$class
pred.versicolor.projected.all <- predict(lda.projected.all, versicolor.projected)$class
pred.virginica.projected.all <- predict(lda.projected.all, virginica.projected)$class

# see the accuracy
acc.setosa.all <- length(which(pred.setosa.projected.all == "setosa"))/length(pred.setosa.projected.all) # accuracy of 1
acc.versicolor.all <- length(which(pred.versicolor.projected.all == "versicolor"))/length(pred.versicolor.projected.all) # accuracy of 1
acc.virginica.all <- length(which(pred.virginica.projected.all == "virginica"))/length(pred.virginica.projected.all) # accuracy of 1

# ------------------------------------------------------------------------------------ #

# Lets see what happens if we just use any two variable without the change of basis

# Use only length
lda.length.only <- lda(Species ~ Sepal.Length + Petal.Length , data = iris)

# Predict on the test data
pred.setosa.length.only <- predict(lda.length.only, data.frame(new.setosa))$class
pred.versicolor.length.only <- predict(lda.length.only, data.frame(new.versicolor))$class 
pred.virginica.length.only <- predict(lda.length.only, data.frame(new.virginica))$class

# see the accuracy
acc.setosa.length.only <- length(which(pred.setosa.length.only == "setosa"))/length(pred.setosa.length.only) # accuracy of 1
acc.versicolor.length.only <- length(which(pred.versicolor.length.only == "versicolor"))/length(pred.versicolor.length.only) # accuracy of 0.933
acc.virginica.length.only <- length(which(pred.virginica.length.only == "virginica"))/length(pred.virginica.length.only) # accuracy of 0.933

# Use only sepal
lda.sepal.only <- lda(Species ~ Sepal.Length + Sepal.Width , data = iris)

# Predict on the test data
pred.setosa.sepal.only <- predict(lda.sepal.only, data.frame(new.setosa))$class
pred.versicolor.sepal.only <- predict(lda.sepal.only, data.frame(new.versicolor))$class 
pred.virginica.sepal.only <- predict(lda.sepal.only, data.frame(new.virginica))$class

# see the accuracy
acc.setosa.sepal.only <- length(which(pred.setosa.sepal.only == "setosa"))/length(pred.setosa.sepal.only) # accuracy of 1
acc.versicolor.sepal.only <- length(which(pred.versicolor.sepal.only == "versicolor"))/length(pred.versicolor.sepal.only) # accuracy of 0.667
acc.virginica.sepal.only <- length(which(pred.virginica.sepal.only == "virginica"))/length(pred.virginica.sepal.only) # accuracy of 0.7667

# Use only width
lda.width.only <- lda(Species ~ Petal.Width + Sepal.Width , data = iris)

# Predict on the test data
pred.setosa.width.only <- predict(lda.width.only, data.frame(new.setosa))$class
pred.versicolor.width.only <- predict(lda.width.only, data.frame(new.versicolor))$class 
pred.virginica.width.only <- predict(lda.width.only, data.frame(new.virginica))$class

# see the accuracy
acc.setosa.width.only <- length(which(pred.setosa.width.only == "setosa"))/length(pred.setosa.width.only) # accuracy of 1
acc.versicolor.width.only <- length(which(pred.versicolor.width.only == "versicolor"))/length(pred.versicolor.width.only) # accuracy of 1
acc.virginica.width.only <- length(which(pred.virginica.width.only == "virginica"))/length(pred.virginica.width.only) # accuracy of 0.9




