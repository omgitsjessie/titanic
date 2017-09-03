library(data.table)
library(ggplot2)
library(dplyr)
train <- fread("~/R Projects - Data Science/titanic/data/train.csv", 
               stringsAsFactors = T)

str(train)

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

#Work on missing data points.  
#1. There are a few without embarkation data
ggplot(train, aes(x = Embarked, y = Fare, Fill = factor(Pclass))) + geom_boxplot()
#Looking at median fare for each class, from each embarkation point, 
#we can prob infer that the missing data points are all from embarkation pt C
train[c(62, 830), "Embarked"] <- "C"

#2. TThere are 177 missing Age points.
#3. Lots of missing rooms.. can we find anything there?
#4. What can we do with Ticket?  
#5. Any change in survival for families of all adults (all siblings; or
#non-familial friends sharing a cabin) relative to families with
#children?
#5b. What about average family age as that predictor?
#6. Possible to infer embarkation from nationality of surname?
#7. Missing Fares - can we build a predictive model for fares, based on
#embarkation point, class, maybe title?
#8. Break up title into more categories?  


summary(train)
glimpse(train)
