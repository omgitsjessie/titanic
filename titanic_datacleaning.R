library(data.table)
library(ggplot2)
library(dplyr)

train <- fread("~/R Projects - Data Science/titanic/data/train.csv", 
               stringsAsFactors = T, na.strings=c(""))
test <- fread("~/R Projects - Data Science/titanic/data/test.csv", 
               stringsAsFactors = T, na.strings=c(""))
test$Survived <- NA
full <- rbind(train, test)
#Cabin
#1. How many rooms, if they're in a cabin?  Regex out spaces and add 1
countSpaces <- function(s) { 
  sapply(gregexpr(" ", s), function(p) {
    sum(p >= 0) 
  }) 
}

#How many cabins did this person/family have? 
full <- transform(full, numRooms = countSpaces(Cabin) + 1)

# What deck were people in (A/B/C/D/E/F/G/NA)?
full$cabinDeck <- as.factor(substring(full$Cabin, 1, 1))

#Name
#1. Ferret out title "rev, miss" etc.
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Title, full$Sex)
rareTitle <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major",
               "Rev", "Sir", "the Countess", "Dona")
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title %in% rareTitle] <- "Rare Title"

#2. Grab last name from the name regex 
full$lastName <- gsub(', .*', '', full$Name)


#Family size
#New column for family size
full$familySize <- 1 + full$SibSp + full$Parch

#TODO: Cluster to create a family ID?  Maybe having a surviving
#family member increases your odds for survival?

#TODO: What about family groups that are unrelated (same cabin, or same ticket)

#Work on missing data points: 
(missingData <- sapply(full,function(x) sum(is.na(x))))
#Missing 2 Embarked 
#Missing 1 Fare
#Missing 263 Age 
#Missing 1014 Cabin + related features - Very sparse.  Not likely to fill in.


#1. There are 2 passengers without embarkation data
ggplot(full, aes(x = Embarked, y = Fare, Fill = factor(Pclass))) + geom_boxplot()
#Looking at median fare for each class, from each embarkation point, 
#we can infer that the missing data points are all from embarkation pt C
full[c(62, 830), "Fare"]  #Each of our NAs paid $80.
full[c(62, 830), ] #These NAs were women traveling in the same 
#cabin, so it's reasonable they embarked from the same port.
full %>% 
  group_by(Embarked, Pclass) %>%
  filter(Pclass == 1) %>%
  summarise(median = median(Fare, na.rm = T),
            mean = mean(Fare, na.rm = T))
#Median fare for Embarked = C is the closest to the fare that
#our NAs paid, so we'll sub in C for them.
full[c(62, 830), "Embarked"] <- "C"



#2. There is 1 passenger without Fare data
ggplot(full, aes(x = Embarked, y = Fare, Fill = factor(Pclass))) + geom_boxplot()
#Looking at median fare for each class, from each embarkation point, 
#passenger 153 (PID 1044) embarked from S, in pclass 3.
#Median fare from Embarked = S and Pclass = 3 is $8.05.  Assign it.
which(is.na(full$Fare))  #the absent Fare is 1044
full[1044,]
full %>% 
  filter(Pclass == 3 & Embarked == "S") %>%
  summarise(median = median(Fare, na.rm = T))
full[1044, "Fare"] <- 8.05



#3. There are 263 missing Age points.  Predict age?  Best to predict
#Age with the 'train' set since age and survivability are likely related.
ageData <- full %>% filter(!is.na(Age) & PassengerId <= 891)
noAgeData <- full %>% filter(is.na(Age) & PassengerId <= 891)


#Since the more highly correlated variables are categorical, predicting with a 
#glm is going to give us a strange distribution (all with the same survival, 
#Pclass etc are going to have the same predicted age).  Additionally, we can't 
#use survivability to predict age for the Test data set since those data are not
#available.  Saturated model:
ageModel <- glm(Age ~ Pclass + Sex + SibSp + Embarked + Title + Fare + 
                      Parch + familySize + familySize*Parch,
                      data=ageData)
summary(ageModel)
#Nonsignificant predictors of age are: 
#sex, Fare.  Reducing the model:
ageModel2 <- glm(Age ~ Pclass + SibSp + Embarked + Title + 
                   familySize + Parch + familySize*Parch, 
                data=ageData)
summary(ageModel2)


#Apply model only to is.na(Age) passengers
for (i in 1:length(noAgeData$Age)) {
    noAgeData[i, "Age"] <- predict(ageModel2)
}
#plot hist of predicted ages versus actual ages to see how off you are
ggplot(ageData, aes(Age)) + geom_histogram(breaks=seq(0, 80, by = 2))
ggplot(noAgeData, aes(Age)) + geom_histogram(breaks=seq(0, 80, by = 2))
#Not perfect, but.. usable.  Better than nothing.  Revisit model later.

#Apply ageModel2 to full age set
for (i in 1:length(full$Age)) {
  if (is.na(full[i, "Age"]) == T) {
    full[i, "Age"] <- predict(ageModel2)
  }
}


#TODO in the future: more feature analysis
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