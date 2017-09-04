#Logistic Regression Approach
train <- full %>% filter(PassengerId <= 891)
test <- full %>% filter(PassengerId > 891)


#Split train data into train and test, to prelim check model fit.
trainmodel <- train %>% filter(PassengerId <= 750)
testmodel <- train %>% filter(PassengerId > 750)

glm.model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
             Fare + Embarked + Title + familySize, 
             family = binomial, 
             data = trainmodel)

testmodel$Survived.glm <- predict(glm.model, type = "response", newdata = testmodel)
#Convert probabilities to 0 or 1?
testmodel$Survived.glm <- round(testmodel$Survived.glm, digits = 0)
mean(testmodel$Survived.glm == testmodel$Survived)
#for this test set, it's giving 88.65248% accuracy

#Create the model from the entire training set.
glm.model.fulltrain <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                 Fare + Embarked + Title + familySize, 
                 family = binomial, 
                 data = train)

#Predict on Test Data
PredictionTest.glm <- predict(glm.model.fulltrain, test, type = "response")
#To satisfy submission requirements, these col names must not change below.
submitTest.glm <- data.frame(PassengerID = test$PassengerId, 
                         Survived = round(PredictionTest, digits = 0))

write.csv(submitTest.glm, 
          file = "~/R Projects - Data Science/titanic/data/glm_1.csv", 
          row.names = F)

#This approach scored 78.469.512% accuracy on the real test; better!!
