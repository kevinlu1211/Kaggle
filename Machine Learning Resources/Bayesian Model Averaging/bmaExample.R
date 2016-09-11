# The sample size
n <- 100

# The 'true' coefficient vector
Beta <- cbind(c(-1.5, 0.45, -3))

# Generate the explanatory variables which have an effect on the outcome
set.seed(1)
X <- cbind(rnorm(n, 0, 1), rnorm(n, 4, 2), rnorm(n, 0.5, 1))
X
# Convert this into probabilities
prob <- 1/(1+exp(-X %*% Beta))
prob
# Generate some uniform numbers. If the elements are smaller than the corresponding elements in the prob vector, then return 1.
set.seed(2)
runis <- runif(n, 0, 1)
runis
y <- ifelse(runis < prob, 1, 0)
y
cbind(y,prob)
# Add the nonsense variables
X <- cbind(X, rpois(n, 3))        # Redundant variable 1 (x4)
X <- cbind(X, rexp(n, 10))        # Redundant variable 2 (x5)
X <- cbind(X, rbeta(n, 3, 10))    # Redundant variable 3 (x6)
X <- cbind(X, rbinom(n, 10, 0.5)) # Redundant variable 4 (x7)
X <- cbind(X, rpois(n, 40))       # Redundant variable 5 (x8)
X <- cbind(X, rgamma(n, 10, 20))  # Redundant variable 6 (x9)
X <- cbind(X, runif(n, 0, 1))     # Redundant variable 7 (x10)


# The BMA
library(BMA)
model <- bic.glm(X, y,  glm.family="binomial", factor.type=FALSE, thresProbne0 = 5, strict = FALSE)

# The frequentist model
model2 <- glm(y~X, family = "binomial")

old.par <- par()
par(mar=c(3,2,3,1.5))
plot(model, mfrow=c(2,5))
par(old.par)

summary(model)
summary(model2)