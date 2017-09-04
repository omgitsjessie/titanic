# Titanic: Machine Learning from Disaster
Predict survival of passengers aboard the Titanic based on passenger characteristics; to introduce and practice with Machine Learning basics.  [Kaggle Competition specifics](https://www.kaggle.com/c/titanic).


## Feature Engineering

Several of these approaches to feature engineering have been borrowed from existing Kaggle kernels, plus a few additions to try to improve accuracy of the predictive models.

### Family Size

### Title

### Number of Rooms

### Missing Data: Age

### Missing Data: Cabin

### Missing Data: Port of Embarkation

## Building a Predictive Model

### Classification trees
The first attempt at using rpart classification trees, since I was familiar with those methods from some other work.  we defined `train$Survived` as the response variable, then built a tree based on a full feature set of variables of interest.  
```
#Define survivability as the response var
response <- train$Survived
rpart.tree <- rpart(response ~ Pclass + Sex + Age + SibSp + Parch + Embarked + 
                    numRooms + cabinDeck + Title + familySize, 
                    data = train, 
                    method = "class", 
                    na.action=na.rpart, 
                    control = rpart.control(minsplit = 5, cp = 0.008))
```
This particular tree's complexity parameter was chosen fairly arbitrarily, and could probably use some tweaking for improved model fit.  Currently this approach yields 77.511% accuracy with the test set.
![rpart Classification Tree](https://github.com/omgitsjessie/titanic/blob/master/figures/class_tree_full_77511perc.png)


## Build final model

### Cross-validation of entire training data set

caret package (Classification and Regression Training)
