# Titanic Kaggle
# import the libraries that are used
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
trainTest$title <- NULL

# Make a fare factor column 
summary(trainTest$Fare)

trainTest$FareFactor <- "0"

# As there is one person with NA for fare it will be imputed with a decision tree
dt.fare <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + FareFactor + Embarked + Title + Age ,
                  data=trainTest[!is.na(trainTest$Fare),], 
                  method="anova")
trainTest$Fare[is.na(trainTest$Fare)] <- predict(dt.fare, trainTest[is.na(trainTest$Fare),])

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

# get all the small families by frequency calculated from SibSp + Parch + 1
famIDs <- famIDs[famIDs$Freq <=2,]

# make these families small as well, although we are losing information by disregarding their family members
trainTest$FamilyID[trainTest$FamilyID %in% famIDs$Var1] <- 'Small'
trainTest$FamilyID <- factor(trainTest$FamilyID)

# Make 3 factor levels to capture the minimum ~ 1st quartile, 1st ~ 3rd quartile, 3rd quartile ~ max
trainTest$FareFactor[trainTest$Fare <= 7.896] <- "Low"
trainTest$FareFactor[trainTest$Fare > 7.896 & trainTest$Fare <= 31.28] <- "Medium"
trainTest$FareFactor[trainTest$Fare > 31.28] <- "High"

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

# get the newTrain data
df <- data.frame(trainTest)
df$FamilySize <- as.factor(df$FamilySize)
train <- df[1:891,]
test <- df[-(1:891),]

### LOGISTIC REGRESSION ###

crossValidate.Logistic <- function(test, fit) {
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
  predicted <- predict(fit, df[-(1:891),], type = "response")
  predicted[predicted < 0.5] <- 0
  predicted[predicted >= 0.5] <- 1
  submit <- data.frame(PassengerId = test$PassengerId, Survived = predicted)
  write.csv(submit, file = paste(fileName,".csv",sep=""), row.names = F)
}

# Now split the train into a train and test fold

# randomize the data first
train <- train[sample(nrow(train)),]

# create the end index to split the train set
endIndex <- as.integer(0.8 * nrow(train))
train_train <- train[1:endIndex,]
train_test <- train[-(1:endIndex),]

# First test which factors are causal so we know what to use in our final model
fit.logistic1 <- glm(Survived ~ Pclass + Sex + Child, data = train_train, family = "binomial")
summary(fit.logistic1)

# Step through the model and discard any insignificant variables
fit.logistic2 <- step(fit.logistic1)
summary(fit.logistic2)

# see what the prediction of the model
print(crossValidate.Logistic(train_test, fit.logistic2))

# create a submission for Kaggle
kaggle.submit.logistic(test, fit.logistic2, "logisticRegression4")

fit.logistic3 <- fit.logistic1 <- glm(Survived ~ Pclass + Sex + Child + FamilySize, data = train_train, family = "binomial")
summary(fit.logistic3)
fit.logistic4 <- step(fit.logistic3)
summary(fit.logistic4)

print(crossValidate.Logistic(train_test, fit.logistic4))

### DECISION TREE ###



















fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, method = "class")







