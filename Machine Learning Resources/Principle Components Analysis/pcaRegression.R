set.seed(2)
x <- 1:100
y <- 20 + 3 * x
e <- rnorm(100, 0, 60)
y <- y + e

# See here that linear models isn't symmetric due to the fact 
# that we assume our data are independent AND have no error
df <- data.frame(x,y)
cov(df)
plot(df)
yx.lm <- lm(y~x)
summary(yx.lm)
lines(x, predict(yx.lm), col="red")
xy.lm <- lm(x~y)
lines(predict(xy.lm), y, col="blue")

plot(x,y)
xyNorm <- cbind(x = x - mean(x), y = y - mean(y))
plot(xyNorm)

# covariance, note that the covariance matrix doesn't change when subtracting the mean,
# as subtracting the mean doesn't change the spread
xyCov <- cov(xyNorm)
cov(df)

eigenValues <- eigen(xyCov)$values
eigenVectors <- eigen(xyCov)$vectors

plot(xyNorm, ylim=c(-200,200), xlim=c(-200,200))
# plot the slope (rise/run) of the vectors
lines(xyNorm[x], eigenVectors[2,1]/eigenVectors[1,1] * xyNorm[x])
lines(xyNorm[x], eigenVectors[2,2]/eigenVectors[1,2] * xyNorm[x])

# the largest eigenValue is the first one
# so that’s our principal component.
# but the principal component is in normalized terms (mean=0)
# and we want it back in real terms like our starting data
# so let’s denormalize it

plot(x,y)
lines(x, (eigenVectors[2,1]/eigenVectors[1,1] * xyNorm[x]) + mean(y))

# what if we bring back our other two regressions?
lines(x, predict(yx.lm), col='red')
lines(predict(xy.lm), y, col='blue')

# now create the new data set using the principle component
t(eigenVectors[,1]) %*% 
