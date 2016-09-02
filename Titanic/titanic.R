# Titanic Kaggle
# import the libraries that are used
library(caret)
library(rpart)
library(randomForest)
library(e1071)

# First get the train and test set
setwd("Documents/Data Science/Kaggle/Titanic")
train <- read.csv("~/Documents/Data Science/Kaggle/Titanic/train.csv")
test <- read.csv("~/Documents/Data Science/Kaggle/Titanic/test.csv")


### DATA CLEANING & FEATURE ENGINEERING ###
# First lets investigate the factors and maybe engineer some new features for prediction
str(train)
str(test)

# need to make the data frames have the same column to trainTest them
test$Survived <- 0
trainTest <- rbind(train,test)
str(trainTest)

# See that some extra information can be extracted from the data, such as the title of the passenger

# Maybe the title of the passenger will be useful? Regardless let's create it as a new feature
trainTest$Title <- sapply(as.character(trainTest$Name), FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
trainTest$Title <- sub(' ', '', trainTest$Title)

# See that there are many "rare" titles some of which are associated with people, aggregate these titles to avoid overfitting if the 
# feature is actually used
table(trainTest$Title)

# now aggregate some of the rare title values
trainTest$Title[trainTest$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
trainTest$Title[trainTest$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
trainTest$Title[trainTest$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(trainTest$Title)
trainTest$Title <- as.factor(trainTest$Title)

# Make a fare factor column 
summary(trainTest$Fare)

# As there is one person with NA for fare it will be imputed with a decision tree
dt.fare <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Embarked + Title + Age ,
                 data=trainTest[!is.na(trainTest$Fare),], 
                 method="anova")
trainTest$Fare[is.na(trainTest$Fare)] <- predict(dt.fare, trainTest[is.na(trainTest$Fare),])

# Make 3 factor levels to capture the minimum ~ 1st quartile, 1st ~ 3rd quartile, 3rd quartile ~ max
trainTest$FareFactor[trainTest$Fare <= 7.896] <- "Low"
trainTest$FareFactor[trainTest$Fare > 7.896 & trainTest$Fare <= 31.28] <- "Medium"
trainTest$FareFactor[trainTest$Fare > 31.28] <- "High"

# Cast it into a factor 
trainTest$FareFactor <- as.factor(trainTest$FareFactor)



# From the Kaggle website we can see that SibSp means the number of siblings/spouses on board, and Parch is the number of parents/children on board
# So create a new feature which is a family ID that records the number of total family members they have on board and the family name
str(trainTest)
trainTest$FamilySize <- trainTest$SibSp + trainTest$Parch + 1

# create new familyID
trainTest$Surname <- sapply(as.character(trainTest$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
trainTest$FamilyID <- paste(as.character(trainTest$FamilySize), trainTest$Surname, sep="")
trainTest$FamilyID[trainTest$FamilySize<=2] <-'Small' 


# there are some families that have an ID of 3FamilyName, but only 1 occurance in the table, it might be because
# people in the same family don't have the same surnames, but we ignore this fact
table(trainTest$FamilyID)
famIDs <- data.frame(table(trainTest$FamilyID))
famIDs
# get all the small families by frequency calculated from SibSp + Parch + 1
famIDs <- famIDs[famIDs$Freq <=2,]

# make these families small as well, although we are losing information by disregarding their family members
trainTest$FamilyID[trainTest$FamilyID %in% famIDs$Var1] <- 'Small'
trainTest$FamilyID <- factor(trainTest$FamilyID)
table(trainTest$FamilyID)


# As we know that children are put onto the lifeboats first create a new feature for this
trainTest$Child <- 0
trainTest$Child[trainTest$Age < 18] <- 1
which(!complete.cases(trainTest))
str(trainTest)
# See that we have some NA's for age, no problem we can impute these values by using a decision tree
dt.age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + FareFactor + FamilyID + Embarked + Title + Child,
                data=trainTest[!is.na(trainTest$Age),], 
                method="anova")
trainTest$Age[is.na(trainTest$Age)] <- predict(dt.age, trainTest[is.na(trainTest$Age),])

# See that there are no incomplete cases now so proceed to split the data into train and set
which(!complete.cases(trainTest))


# Check out the Cabin variable, but it seems like there are a lot samples that are missing the Cabin feature, and after understanding how the cabins where
# the cabins were place it seems as if it is highly correlated with Pclass (which is a proxy for social-economic class) so adding Cabin wouldn't really help much
# regardless I will try to see if it makes a difference
table(trainTest$Cabin)
aggregate(Survived ~ Cabin, data = trainTest, FUN=length)

# But it does seem that people who don't have a Cabin assigned to them have a lower survival rate than 20% which means that we might be able to squeeze a bit more of 
# the data
aggregate(Survived ~ Cabin, data = trainTest, FUN=sum)

# First turn the column into a string for easier manipulation
trainTest$Cabin <- as.character(trainTest$Cabin)
trainTest$Cabin[which(trainTest$Cabin == "")] <- "U"

# Get rid of the spaces as we just want the first character
trainTest$Cabin <- gsub(' ', '', trainTest$Cabin)
table(trainTest$Cabin)

# now grab the first letter
trainTest$CabinLetter <- sapply(trainTest$Cabin, function(x) strsplit(x,split="")[[1]][1])
trainTest$CabinLetter <- as.matrix(trainTest$CabinLetter)
dim(trainTest$CabinLetter)

# cast it back into a factor so that we can use it
trainTest$CabinLetter <- as.factor(trainTest$CabinLetter)

# get the newTrain data
df <- data.frame(trainTest)

# Convert the features to factors
df$FamilySize <- as.factor(df$FamilySize)
df$Pclass <- as.factor(df$Pclass)
df$Survived <- as.factor(df$Survived)
df$Child <- as.factor(df$Survived)

train <- df[1:891,]
test <- df[-(1:891),]

# Now split the train into a train and test fold

# randomize the data first
train <- train[sample(nrow(train)),]

# create the end index to split the train set
endIndex <- as.integer(0.8 * nrow(train))
train_train <- train[1:endIndex,]
train_test <- train[-(1:endIndex),]


### LOGISTIC REGRESSION ###

crossValidate.logistic <- function(test, fit) {
  actual <- array(test[,"Survived"])
  predicted <- predict(fit, test, type = "response")
  predicted[predicted < 0.5] <- 0
  predicted[predicted >= 0.5] <- 1
  correctCount <- 0
  for(i in 1:length(predicted)) {
    if(predicted[i] == actual[i]) {
      correctCount <- correctCount + 1
    }  
  }
  print("Number of correct classifications:")
  print(correctCount)
  
  print("Number of samples:")
  print(nrow(test))
  
  accuracy <- correctCount/nrow(test)
  return(accuracy)
}
kaggle.submit.logistic <- function(test, fit, fileName) {
  predicted <- predict(fit, test, type = "response")
  predicted[predicted < 0.5] <- 0
  predicted[predicted >= 0.5] <- 1
  submit <- data.frame(PassengerId = test$PassengerId, Survived = predicted)
  write.csv(submit, file = paste("Submissions/",fileName,".csv",sep=""), row.names = F)
}


# First test which factors are causal so we know what to use in our final model
fit.logistic <- glm(Survived ~ Pclass + Sex + Child, data = train_train, family = "binomial")
summary(fit.logistic)

# Step through the model and discard any insignificant variables
fit.logistic <- step(fit.logistic)
summary(fit.logistic)

# see what the prediction of the model
print(crossValidate.Logistic(train_test, fit.logistic))

# create a submission for Kaggle
kaggle.submit.logistic(test, fit.logistic, "logisticRegression4")

fit.logistic <- glm(Survived ~ Pclass + Sex + Child + FamilySize, data = train_train, family = "binomial")
summary(fit.logistic)
fit.logistic <- step(fit.logistic)
summary(fit.logistic)

print(crossValidate.Logistic(train_test, fit.logistic))
kaggle.submit.logistic(test, fit.logistic, "logisticRegression5")

fit.logistic <- glm(Survived ~ Pclass + Sex + Child + FamilySize + Title + FareFactor, data = train_train, family = "binomial")
summary(fit.logistic)

fit.logistic <- step(fit.logistic)
summary(fit.logistic)

print(crossValidate.Logistic(train_test, fit.logistic))
kaggle.submit.logistic(test, fit.logistic, "logisticRegression6")

fit.logistic <- glm(Survived ~ Pclass + Sex + Child + FamilySize + Title + FareFactor + Embarked, data = train_train, family = "binomial")
summary(fit.logistic)

fit.logistic.stepped <- step(fit.logistic)
summary(fit.logistic)

# See that there isn't much of an improvement, but I noticed that there is a Cabin variable that I didn't use maybe I should engineer some new features from it
print(crossValidate.Logistic(train_test, fit.logistic))

fit.logistic <- glm(Survived ~ Pclass + Child + FamilySize + Sex + FareFactor, data = train_train, family = "binomial")
summary(fit.logistic)

# Check p-value of the model
pchisq(943.08 - deviance(fit.logistic), 711-698, lower.tail = FALSE)

fit.logistic.stepped <- step(fit.logistic)
summary(fit.logistic)

# See that the logistic regression has an accuracy that has capped around ~0.8
print(crossValidate.Logistic(train_test, fit.logistic))

# Use the new factor and see how it fares
fit.logistic <- glm(Survived ~ Pclass + Sex + Age + FamilySize + CabinLetter, data = train_train, family = "binomial")
summary(fit.logistic)

fit.logistic.stepped <- step(fit.logistic)
summary(fit.logistic.stepped)

# Sees like there is an increase in the accuracy
print(crossValidate.Logistic(train_test, fit.logistic.stepped))
kaggle.submit.logistic(test, fit.logistic, "logisticRegression7")


### DECISION TREE ###
library(rattle)
library(rpart.plot)
library(RColorBrewer)

crossValidate.decisionTree <- function(test, fit) {
  actual <- array(test[,"Survived"])
  predicted <- predict(fit, test, type = "class")
  correctCount <- 0
  for(i in 1:length(predicted)) {
    if(predicted[i] == actual[i]) {
      correctCount <- correctCount + 1
    }  
  }
  print("Number of correct classifications:")
  print(correctCount)
  
  print("Number of samples:")
  print(nrow(test))
  
  accuracy <- correctCount/nrow(test)
  return(accuracy)
}

kaggle.submit.decisionTree <- function(test, fit, fileName) {
  predicted <- predict(fit, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = predicted)
  write.csv(submit, file = paste("Submissions/",fileName,".csv",sep=""), row.names = F)
}

findBestCP <- function(cps, fit.dt) {
  bestAccuracy <- 0
  bestCP <- 0
  bestFit <- NULL
  for(cp in cps) {
    fit.dt.pruned <- prune(fit.dt, cp = cp)
    
    # Compare the accuracy of un-pruned and pruned tree
    
    accuracy <- crossValidate.decisionTree(train_test, fit.dt.pruned)
    if (accuracy > bestAccuracy) {
      bestAccuracy <- accuracy
      bestCP <- cp
      bestFit <- fit.dt.pruned
    }
    print(paste("Accuracy of tree with cp:", cp))
    print(accuracy)
  }
  return(list(accuracy=bestAccuracy, cp = bestCP, fit = bestFit))
}
# First vallina model with no use of cross validation and no use of complexity parameter and we have included high correlated features?
fit.dt <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train_train, method = "class")
crossValidate.decisionTree(train_test, fit.dt)
summary(fit.dt)
plotcp(fit.dt)
fancyRpartPlot(fit.dt)
# kaggle.submit.decisionTree(test, fit.dt, "decisionTree1")

# Fit a full tree = 
fit.dt <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + FareFactor + Embarked + Title + FamilySize + FamilyID, data = train_train, method = "class",
                control = rpart.control(cp = 0, xval = 10))
summary(fit.dt)
plotcp(fit.dt)
printcp(fit.dt)
fancyRpartPlot(fit.dt)
cps <- printcp(fit.dt)[,1]



best.dt.result <- findBestCP(cps, fit.dt)
best.dt.result$accuracy
best.dt.result$cp
best.dt.result$fit
kaggle.submit.decisionTree(test, best.dt.result$fit, "decisionTree2")

### RANDOM FORESTS ###

crossValidate.randomForest <- function(test, fit) {
  actual <- array(test[,"Survived"])
  predicted <- predict(fit, test)
  correctCount <- 0
  for(i in 1:length(predicted)) {
    if(predicted[i] == actual[i]) {
      correctCount <- correctCount + 1
    }  
  }
  print("Number of correct classifications:")
  print(correctCount)
  
  print("Number of samples:")
  print(nrow(test))
  
  accuracy <- correctCount/nrow(test)
  return(accuracy)
}

fit.rf <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + FamilySize + FareFactor + Title + FamilyID ,
                    data=train_train, 
                    importance=TRUE, 
                    ntree=2000)

print(crossValidate.randomForest(train_test, fit.rf))


# use conditional inference trees 
library(party)

crossValidate.conditionalInferenceTrees <- function(test, fit) {
  actual <- array(test[,"Survived"])
  predicted <- predict(fit, test, OOB=TRUE, type = "response")
  correctCount <- 0
  for(i in 1:length(predicted)) {
    if(predicted[i] == actual[i]) {
      correctCount <- correctCount + 1
    }  
  }
  print("Number of correct classifications:")
  print(correctCount)
  
  print("Number of samples:")
  print(nrow(test))
  
  accuracy <- correctCount/nrow(test)
  return(accuracy)
}
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
print(crossValidate.conditionalInferenceTrees(train_test, fit))
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "secondForest.csv", row.names = F)


### SUPPORT VECTOR MACHINES ###
library(e1071)

# Simple vanilla use of SVMs
fit.svm <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + FareFactor + FamilySize + FamilyID + Child, data = train_train)
predict(fit.svm, train_test)

crossValidate.supportVectorMachine <- function(fit, test) {
  actual <- array(test[,"Survived"])
  predicted <- predict(fit, test)
  correctCount <- 0
  for(i in 1:length(predicted)) {
    if(predicted[i] == actual[i]) {
      correctCount <- correctCount + 1
    }  
  }
  print("Number of correct classifications:")
  print(correctCount)
  
  print("Number of samples:")
  print(nrow(test))
  
  accuracy <- correctCount/nrow(test)
  return(accuracy)
}

crossValidate.supportVectorMachine(fit.svm, train_test)

# Now do some cross validation to find the best cost (default is 1)
powers <- seq(-5, 5, 0.5)
costs <- sapply(powers, FUN=function(x) 10^x)

findBestCost <- function(costs, train, test) {
  bestAccuracy <- 0
  bestCost <- 0
  bestFit <- NULL
  for(cost in costs) {
    fit.svm <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + FareFactor + FamilySize + FamilyID + Child, data = train, cost = cost)
    
    # Compare the accuracy of svms with different costs
    
    accuracy <- crossValidate.supportVectorMachine(fit.svm,test)
    if (accuracy > bestAccuracy) {
      bestAccuracy <- accuracy
      bestCost <- cost
      bestFit <- fit.svm
    }
    print(paste("Accuracy of svm with cost:", cost))
    print(accuracy)
  }
  return(list(accuracy=bestAccuracy, cost = bestCost, fit = bestFit))
}

best.svm.result <- findBestCost(costs, train_train, train_test)

kaggle.submit.supportVectorMachine <- function(fit, test, fileName) {
  predicted <- predict(fit, test)
  submit <- data.frame(PassengerId = test$PassengerId, Survived = predicted)
  write.csv(submit, file = paste("Submissions/",fileName,".csv",sep=""), row.names = F)
}

best.result.svm <- findBestCost(costs, train_train, train_test)

fit.svm <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + FareFactor + FamilySize + FamilyID + Child, data = train, cost = best.result.svm$cost)
kaggle.submit.supportVectorMachine(fit.svm, test, "svm1")

### Linear Discriminant Analysis ###

lda.fit <- lda(Survived ~ as.numeric(Pclass) + Age, as.numeric(FamilySize), data = train_train)
?lda
