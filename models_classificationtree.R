#Classification Tree Approach
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Pull in 'full' set following processing in titanic_datacleaning.R.  
#TODO - when cleaning is finalized, move that data to github and read it in here
#TODO - Can you bootstramp sampling to get a better fit model?

#Pull out train data from the full set.
train <- full %>% filter(PassengerId <= 891)
test <- full %>% filter(PassengerId > 891)


#Split train data into train and test, to prelim check model fit.
trainmodel <- train %>% filter(PassengerId <= 750)
testmodel <- train %>% filter(PassengerId > 750)

#Classification Tree prediction.
#Define survivability as the response var
response <- trainmodel$Survived
#Grow tree
rpart.tree <- rpart(response ~ Pclass + Sex + Age + SibSp + Parch + Embarked + 
                      numRooms + cabinDeck + Title + familySize, 
                    data = trainmodel, 
                    method = "class", 
                    na.action=na.rpart, 
                    control = rpart.control(minsplit = 5, cp = 0.008))
#Plot tree
plot_fulltree <- fancyRpartPlot(rpart.tree)

#Predict on your set aside test set.
Prediction <- predict(rpart.tree, testmodel, type = "class")
submit <- data.frame(PassengerID = testmodel$PassengerId) 
submit$SurvivedPrediction <- Prediction
submit$SurvivedActual = as.factor(testmodel$Survived) 
submit$Accuracy = (submit$SurvivedPrediction == submit$SurvivedActual)
mean(submit$Accuracy)
  #87% on my sampled training/test set!


#*******************************************************************************
#Predict on the Real Test Set

#TODO - grow tree based on the entire training set
response2 <- train$Survived
rpart.tree2 <- rpart(response2 ~ Pclass + Sex + Age + SibSp + Parch + Embarked + 
                      numRooms + cabinDeck + Title + familySize, 
                    data = train, 
                    method = "class", 
                    na.action=na.rpart, 
                    control = rpart.control(minsplit = 5, cp = 0.008))
#Plot tree
plot_fulltree <- fancyRpartPlot(rpart.tree2)

#Predict on Test Data
PredictionTest <- predict(rpart.tree2, test, type = "class")
#To satisfy submission requirements, these col names must not change below.
submitTest <- data.frame(PassengerID = test$PassengerId, 
                         Survived = PredictionTest)
write.csv(submitTest, 
          file = "~/R Projects - Data Science/titanic/data/classtree_2.csv", 
          row.names = F)

#This approach scored 77.512% accuracy on the real test; not great.  Keep at it.
