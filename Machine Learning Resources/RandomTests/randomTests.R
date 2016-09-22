# See here that if the model only uses X1 then regardless of what other columns of the test data.frame has it
# it will only use the X1 column for prediction
x <- matrix(c(1,3,14,
              2,234,43,
              3,1234,1234,
              4,1234,1234,
              5,1234123,4123412,
              10,1234,1234), 6, 3, byrow = TRUE)
x
y <- c(2,4,6,8,10,20)
df <- data.frame(x,y)
df
fit <- lm(y~X1, data=df[-(5:6),])
summary(fit)

predict(fit,df[5:6,])
df[5:6,]
