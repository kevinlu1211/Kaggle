# Titanic Kaggle

# First get the train and test set
setwd("Documents/Data Science/Kaggle/Titanic")
train <- read.csv("~/Documents/Data Science/Kaggle/Titanic/train.csv")
test <- read.csv("~/Documents/Data Science/Kaggle/Titanic/test.csv")


# First lets investigate the factors and maybe engineer some new features for prediction
str(train)
str(test)

# need to make the data frames have the same column to trainTestne them
test$Survived <- 0
trainTest <- rbind(train,test)
str(trainTest)
# See that some extra information can be extracted from the data, such as the title of the passenger

# Basic survival proportion
survivedTable <- table(with(train, Survived))
prop.table(survivedTable)

# Maybe the title of the passenger will be useful? Regardless let's create it as a new feature
trainTest$Title <- sapply(as.character(trainTest$Name), FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
trainTest$Title <- sub(' ', '', trainTest$Title)

# See that there are many "rare" titles some of which are associated with people who have some society status
# which is probably not going to be useful or lead to overfitting
table(trainTest$Title)

# now aggregate some of the rare title values
trainTest$Title[trainTest$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
trainTest$Title[trainTest$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
trainTest$Title[trainTest$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(trainTest$Title)
trainTest$title <- NULL

# Make a fare factor column 
summary(trainTest$Fare)

trainTest$FareFactor <- "0"

# As there is one person with NA for fare it will be imputed with a decision tree
fit.fare <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + FareFactor + Embarked + Title + Age ,
                  data=trainTest[!is.na(trainTest$Fare),], 
                  method="anova")
trainTest$Fare[is.na(trainTest$Fare)] <- predict(fit.fare, trainTest[is.na(trainTest$Fare),])


# Make 3 factor levels to capture the minimum ~ 1st quartile, 1st ~ 3rd quartile, 3rd quartile ~ max
trainTest$FareFactor[trainTest$Fare <= 7.896] <- "Low"
trainTest$FareFactor[trainTest$Fare > 7.896 & trainTest$Fare <= 31.28] <- "Medium"
trainTest$FareFactor[trainTest$Fare > 31.28] <- "High"

# As we know that children are put onto the lifeboats first create a new feature for this
trainTest$Child <- 0
trainTest$Child[trainTest$Age < 18] <- 1
which(!complete.cases(trainTest))

# See that we have some NA's for age, no problem we can impute these values by using a decision tree
library(rpart)
fit.age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + FareFactor + Embarked + Title ,
                data=trainTest[!is.na(trainTest$Age),], 
                method="anova")
trainTest$Age[is.na(trainTest$Age)] <- predict(fit.age, trainTest[is.na(trainTest$Age),])


which(!complete.cases(trainTest))

# get the newTrain data
newTrain <- trainTest[1:891,]
newTest <- trainTest[-(1:891),]
dim(newTrain)
dim(newTest)


### LOGISTIC REGRESSION ###

# First test which factors are causal so we know what to use in our final model
fit <- glm(Survived ~ Pclass + Sex + Age + FareFactor + SibSp + Parch + Embarked, data = newTrain, family = "binomial")
fit1 <- step(fit)
# Hence we can determine that Pclass + Sex + Age + SibSp are the causal ones
summary(fit1)

logistic.regression <- function(train, test) {
  fit <- glm(Survived ~ Pclass + Sex + Age + SibSp, data = train, family = "binomial")
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
  accuracy <- correctCount/nrow(testData)
  return(accuracy)
}

# Perform 10 fold cross validation
crossValidate.Logistic <- function(k = 10, data, folds) {
  accuracies <- c()
  for(i in 1:k){
    #Segment data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- newTrain[testIndexes, ]
    trainData <- newTrain[-testIndexes, ]
    fit <- logistic.regression(trainData, testData)
    accuracy <- fit$accuracy
    accuracies <- c(accuracies, accuracy)
  }
  print(accuracies)
  return(mean(accuracies))
}

folds <- cut(seq(1,nrow(newTrain)),breaks=10,labels=FALSE)

print(crossValidate.Logistic(10, newTrain, folds))

# create a submission for Kaggle
predicted <- predict(fit1, newTest, type = "response")
predicted
predicted[predicted < 0.5] <- 0
predicted[predicted >= 0.5] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = predicted)
submit
write.csv(submit, file = "logisticRegression.R", row.names = F)

### DECISION TREE ###








