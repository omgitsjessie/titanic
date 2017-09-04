library(data.table)
library(ggplot2)
library(dplyr)
require(tree) 
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
train <- fread("~/R Projects - Data Science/titanic/data/train.csv", 
               stringsAsFactors = T, na.strings=c(""))
test <- fread("~/R Projects - Data Science/titanic/data/test.csv", 
               stringsAsFactors = T, na.strings=c(""))

#Cabin
#1. How many rooms, if they're in a cabin?  Regex out spaces and add 1
countSpaces <- function(s) { 
  sapply(gregexpr(" ", s), function(p) {
    sum(p >= 0) 
  }) 
}

#This changes every value of numRooms to be the number of spaces + 1 in 
#the cabin col.  
train <- transform(train, numRooms = countSpaces(Cabin) + 1)
#This fixes numRooms to correct for missing data.
for (i in 1:length(train$Cabin)) {
  if (train[i, "Cabin"] == "") {
    train[i, "numRooms"] <- NA
  }
}

# What deck were people in?
train$cabinDeck <- substring(train$Cabin, 1, 1)
for (i in 1:length(train$cabinDeck)) {
  if (train[i, "cabinDeck"] == "") {
    train[i, "cabinDeck"] <- NA
  }
}
train$cabinDeck <- as.factor(train$cabinDeck)

#Fare
#1. plot out age vs fare.  Nothing obvious here.
ggplot(train, aes(Age, Fare)) + geom_point()

#Name
#1. Ferret out title "rev, miss" etc.
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
table(train$Title, train$Sex)
rareTitle <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major",
               "Rev", "Sir", "the Countess")
train$Title[train$Title == "Mme"] <- "Mrs"
train$Title[train$Title == "Mlle"] <- "Miss"
train$Title[train$Title == "Ms"] <- "Miss"
train$Title[train$Title %in% rareTitle] <- "Rare Title"

#2. Grab last name from the name regex 
train$lastName <- gsub(', .*', '', train$Name)


#Family size
#New column for family size
train$familySize <- 1 + train$SibSp + train$Parch
ggplot(train, aes(familySize, Fare)) + geom_point()

#TODO: Cluster to create a family ID?

#TODO: What about family groups that are unrelated (same cabin, or same ticket)

#Work on missing data points. 2 Embarked, 177 Age, 687 Cabin/numrooms/cabinDeck.
missingData <- sapply(train,function(x) sum(is.na(x)))

#1. There are 2 passengers without embarkation data
ggplot(train, aes(x = Embarked, y = Fare, Fill = factor(Pclass))) + geom_boxplot()
#Looking at median fare for each class, from each embarkation point, 
#we can infer that the missing data points are all from embarkation pt C
train[c(62, 830), "Embarked"] <- "C"

#2. There are 177 missing Age points.  Predict age?
ageData <- train %>% filter(!is.na(Age))
noAgeData <- train %>% filter(is.na(Age))
ageModel <- glm(Age ~ Survived + Pclass + SibSp, 
               data=ageData)
summary(ageModel)
#Apply model only to is.na(Age) passengers
for (i in 1:length(noAgeData$Age)) {
    noAgeData[i, "Age"] <- predict(ageModel, newdata = noAgeData[i,])
}
#plot hist of predicted ages versus actual ages to see how off you are
ggplot(ageData, aes(Age)) + geom_histogram(breaks=seq(0, 80, by = 2))
ggplot(noAgeData, aes(Age)) + geom_histogram(breaks=seq(0, 80, by = 2))
#Not perfect, but.. usable.  Better than nothing.  Revisit model later.
#Apply that model to train age set
for (i in 1:length(train$Age)) {
  if (is.na(train[i, "Age"]) == T) {
    train[i, "Age"] <- predict(ageModel)
  }
}

#3. 687 of missing rooms.. can we find anything there?
#4. What can we do with Ticket?  
#5. Any change in survival for families of all adults (all siblings; or
#non-familial friends sharing a cabin) relative to families with
#children?
#5b. What about average family age as that predictor?
#6. Possible to infer embarkation from nationality of surname?
#7. Missing Fares - can we build a predictive model for fares, based on
#embarkation point, class, maybe title?
#8. Break up title into more categories?


#9.  Married but travelling alone? Over 18, sibsp = 0, married
    # train$marriedSolo <- 0
    # for (i in 1:length(train$marriedSolo)) {
    #   if (train[i, "Age"] > 18 && train[i, "SibSp"] == 0 && train[i, "Title"] == "Mrs") {
    #     train[i, "marriedSolo"] <- 1
    #   }
    # }



#TODO - Can you bootstramp sampling to get a better fit model?

#Split train data into train and test, to prelim check model fit.
trainmodel <- train %>% filter(PassengerId <= 750)
testmodel <- train %>% filter(PassengerId > 750)

#Classification Tree prediction.
#Define survivability as the response var
response <- train$Survived
#Grow tree
rpart.tree <- rpart(response ~ Pclass + Sex + Age + SibSp + Parch + Embarked + 
                    numRooms + cabinDeck + Title + familySize, 
                    data = train, 
                    method = "class", 
                    na.action=na.rpart, 
                    control = rpart.control(minsplit = 5, cp = 0.008))
#Plot tree
plot_fulltree <- fancyRpartPlot(rpart.tree)

#Predict on your set aside test set.
Prediction <- predict(rpart.tree, testmodel, type = "class")
submit <- data.frame(PassengerID = testmodel$PassengerId, 
          SurvivedPrediction = Prediction,
          SurvivedActual = as.factor(testmodel$Survived), 
          Accuracy = (submit$SurvivedPrediction == submit$SurvivedActual))
mean(submit$Accuracy)

#*******************************************************************************
#Predict on the Real Test Set

##Clean the Real Test Set.
#1. How many rooms, if they're in a cabin?  Regex out spaces and add 1.  This 
#changes every value of numRooms to be the number of ' ' + 1 in the cabin col.  
test <- transform(test, numRooms = countSpaces(Cabin) + 1)

# What deck were people in?
test$cabinDeck <- substring(test$Cabin, 1, 1)
test$cabinDeck <- as.factor(test$cabinDeck)

#Name
#1. Ferret out title "rev, miss" etc.
test$Title <- gsub('(.*, )|(\\..*)', '', test$Name)
table(test$Title, test$Sex)
rareTitle <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major",
               "Rev", "Sir", "the Countess", "Dona")
test$Title[test$Title == "Mme"] <- "Mrs"
test$Title[test$Title == "Mlle"] <- "Miss"
test$Title[test$Title == "Ms"] <- "Miss"
test$Title[test$Title %in% rareTitle] <- "Rare Title"

#2. Grab last name from the name regex 
test$lastName <- gsub(', .*', '', test$Name)

#Family size - create a new column for family size
test$familySize <- 1 + test$SibSp + test$Parch

#Work on missing data points. 86 Age, 327 Cabin/numrooms/cabinDeck, 1 Fare.
missingData <- sapply(test,function(x) sum(is.na(x)))

#1. There is 1 passengers without Fare data
ggplot(test, aes(x = Embarked, y = Fare, Fill = factor(Pclass))) + geom_boxplot()
#Looking at median fare for each class, from each embarkation point, 
#passenger 153 (PID 1044) embarked from S, in pclass 3.
#Median fare from Embarked = S and Pclass = 3 is $8.05.  Assign it.
test[153,]
test %>% 
  filter(Pclass == 3 & Embarked == "S") %>%
  summarise(median = median(Fare, na.rm = T))
test[c(153), "Fare"] <- 8.05

#2. There are 86 missing Age points.  Predict age from Training Age model
#Apply model only to is.na(Age) passengers
for (i in 1:length(test$Age)) {
  if (Age = NA) {
    test[i, "Age"] <- predict(ageModel)
  }
}


#There is a new Title in the mix here, so change it to 'Rare Title'
which(test$Title == "Dona")
test$Title[415] <- "Rare Title"
#Predict on Test Data
PredictionTest <- predict(rpart.tree, test, type = "class")
#To satisfy submission requirements, these col names must not change below.
submitTest <- data.frame(PassengerID = test$PassengerId, 
                     Survived = PredictionTest)
write.csv(submitTest, 
    file = "~/R Projects - Data Science/titanic/data/SubmittedPredictions.csv", 
    row.names = F)

#This iteration scored 77.511% accuracy; not great.  Keep at it.

