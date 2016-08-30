### Use the Caret Library for spot checking ###

### Create a new train/test from the original train set, this function uses boostrap resampling
inTraining <- createDataPartition(train$Survived, p = .75, list = FALSE)
train$Survived <- as.factor(train$Survived)
train_train <- train[inTraining,]
train_test <- train[-inTraining,]

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
metric <- "Accuracy"
seed <- 101

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(Survived~.- CabinLetter - PassengerId - Name - Ticket, data=train_train, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(Survived~ - CabinLetter - PassengerId - Name - Ticket., data=train_train, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Survived~. - CabinLetter - PassengerId - Name - Ticket, data=train_train, method="gbm", metric=metric, trControl=control, verbose=FALSE)