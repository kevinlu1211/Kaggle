# Support vector machines
library(e1071)
set.seed(1)

# simulate a 20x2 matrix 
x <- matrix(rnorm(20*2), ncol=2)

# simulate the classes
y <- c(rep(-1,10), rep(1,10)) 

# Make the classes more differentiable 
x[y==1,] <- x[y==1,] + 1

# Plot the graphs and see if there are linearly separable
plot(x, col = (3-y))

# make it into a data frame so that we can fit the svm
dat = data.frame(x=x, y=as.factor(y))
head(dat)

# Fit the soft-margin linear svm model, note that e1071's svm function uses a different parameterization to the
# one used in Introduction t Statistical Learning (ISL), the relationship is:
#
# C_{e1071} = numberOfTrainingSamples/{C_ISL}
#
# The consequence of this that in ISL: 
# lower cost C_{ISL} => lower total error => Minimum margin between the support vectors decreases
#
# because the constraint for the margin is:
#
# y_i * (b_0 + b_1*x_{i1} + ... b_p*x{ip}) >= M(1-error_i) where M is the margin (1.1)
#
# If the cost C is low (remember that C is the sum of all error_i), then it means that M must be small so that the constraint is satisfied
# To understand this lets take the most extreme case: 
# Imagine that we have a cost C = number of samples in the training set 
# What this means is that, regardless of what value the margin M takes, we are able to pick an error that allows the 
# constraint (1.1) to be satisfied since we can just set error_i to 1 meaning that M(1-error_i) will always be 0 regardless.
# 
# Hence the effect of the cost C_{e1071} used in svm() has that:
# As C_{e1071} decreases, C_{ISL} increases, therefore:
# lower cost C_{e1071} =>  higher cost C_{ISL} => higher total allowable error => Minimum margin between the support vectors increases
# 
# So we can see that what the cost does is a tradeoff between, underfitting and overfitting, if we choose a high C then we will most likely
# overfit due to the fact that there will a small margin (and remember that the margin width is our confidence in classifying new data, the
# greater the margin with the higher the confidence). If we choose a low C then it means that the margin will be very wide, but most of our 
# training data will be inside the margin, therefore we will be underfitting since the model will not be able to distiguish between different
# classes.

svm.fit <- svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)

# Plot the svm note here that the X's are the support vectors, and the colors of the O's and X's are which class the vector belongs in
plot(svm.fit, dat)

# Find the support vectors 
svm.fit$index

# We see here that there were seven support vectors, four in one class and three in the other
# We note here that the support vectors are the vectors that are inside the margin
# Here we can deduce that if we decrease the cost, number of support vectors will increase, due to the increase in margin
summary(svm.fit)

svm.fit <- svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)

# We see here that there are more support vectors are we increased the margin width
plot(svm.fit, dat)
svm.fit$index

# So how do we pick the optimal value of C? Through cross validation!
set.seed(1)

# use the inbuilt function to do cross validation
tune.out <- tune(svm,y ~.,data=dat,kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
best.svm.mod <- tune.out$best.model



