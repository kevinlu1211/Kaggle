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
pred.setosa <- predict(pca, new.setosa)
pred.versicolor <- predict(pca, new.versicolor)
pred.virginica <- predict(pca, new.virginica)

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
# 1. Used the pca function to find the eigenvectors needed for the change of basis of coordinates
# 2. Used the predict function of prcomp to map new testing data to the new coordinates

# Now lets see how we can use PCA for dimensionality reduction

# First 

