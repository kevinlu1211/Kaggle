# importing the data files
setwd("Documents/Data Science/Kaggle/Titanic")
train <- read.csv("~/Documents/Data Science/Kaggle/Titanic/train.csv")
test <- read.csv("~/Documents/Data Science/Kaggle/Titanic/test.csv")
str(train)
str(test)

# see what percentage of passengers survived
table(with(train, Survived))

# better to use proportions
prop.table(table(with(train, Survived)))

# since ~61% of them die, predict that they all die
test$Survived <- rep(0,nrow(test))

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyalldie.csv", row.names = F)

# now lets see what it looks like when we have sex included
str(train)

# use prop.table to see what the proportion looks like
# 1 is for summing rows, 2 is for summing columns
prop.table(table(train$Sex, train$Survived), 1)

test$Survived <- 0

# These are equivalent
test$Survived[test$Sex == "female"] <- 1
# test$Survived[which(test$Sex == "female")] <- 1

# create a new feature to create a Child column 
train$Child <- 0
train$Child[train$Age < 18] <- 1

# breakdown of the subset of people that survived
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

# breakdown of the subset 
aggregate(Survived ~ Child + Sex, data = train, FUN=length)

# breakdown of the subset proportion
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})

# fare breakdown for the number of passengers 
aggregate(PassengerId ~ Fare, data = train, FUN=length)

# create a new factor feature 
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Child + Sex + Pclass, data = train, FUN=function(x) {sum(x)/length(x)})
aggregate(Survived ~ Child + Sex + Pclass, data = train, FUN=length)
aggregate(Survived ~ Child + Sex + Pclass, data = train, FUN=sum)
aggregate(PassengerId ~ Pclass + Fare2, data = train, FUN = length)

test$Survived <- 0
test$Survived[test$Pclass == 1 & test$Sex == 'female'] <- 1
test$Survived[test$Pclass == 1 & test$Child == 1 & test$Sex == 'male'] <- 1
test$Survived[test$Pclass == 2 & test$Sex == 'female'] <- 1
test$Survived[test$Pclass == 2 & test$Child == 1 & test$Sex == 'male'] <- 1
test$Survived[test$Pclass == 3 & test$Child == 1 & test$Sex == 'female'] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "childSexPclass.csv", row.names =F)

# although this would be more effective if we used a decision tree
library(rpart) ; library(rattle) ; library(rpart.plot) ; library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "decisionTree.csv", row.names = FALSE)

# feature engineering 
test$Survived <- NA
train$Child <- NULL
train$Fare2 <- NULL
combi <- rbind(train,test)

# cast the name factor into a string
combi$Name <- as.character(combi$Name)
combi$Name[1]

# now split the string and find the titles for each passenger

# method 1 use a for loop
titles <- c()
for (name in combi$Name) {
  titles <- c(titles, strsplit(name, split = '[,.]')[[1]][2])
}
titles
# method 2 use sapply
titles <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- titles
str(combi)
combi$title <- NULL

# strip the spaces
combi$Title <- sub(' ', '', combi$Title)

# now aggregate some of the rare title values
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(combi$Title)

# convert the strings back into factors
combi$Title <- as.factor(combi$Title)
table(combi$Title)

# create familySize feature
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# create new familyID
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize<=2] <-'Small' 
# there are some families that have an ID of 3FamilyName, but only 1 occurance in the table, it might be because
# people in the same family don't have the same surnames, but we ignore this fact


table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))

# get all the small families by frequency calculated from SibSp + Parch + 1
famIDs <- famIDs[famIDs$Freq <=2,]

# make these families small as well, although we are losing information by disregarding their family members
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# define new train and test sets
train <- combi[1:891,]
test <- combi[-(1:891),]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, method = "class")
prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "decisionTree2.csv", row.names = FALSE)

# get "predict" the people that don't have age with the decision tree
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")

# clean the data so that randomForest library works
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Embarked)
combi$Embarked[which(combi$Embarked == '')] <- "S"
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm = TRUE)


combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[-(1:891),]

# use a randomForest
library(randomForest)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = F)

# use conditional inference trees 
install.packages('party')
library(party)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "secondForest.csv", row.names = F)
