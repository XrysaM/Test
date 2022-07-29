#this is a test script on my test project

#Rpubs - Classification with Decision Trees
#C4.5 - C5.0
#C4.5
library(RWeka)
library(caret)
data(iris)
#training set
set.seed(1958)  # set a seed to get replicable results
train <- createFolds(iris$Species, k=10)
C45Fit <- train(Species ~., method="J48", data=iris,
                tuneLength = 5,
                trControl = trainControl(
                  method="cv", indexOut=train))

C45Fit$finalModel
summary(C45Fit$finalModel)
plot(C45Fit$finalModel)#exei la8aki sta speciesnames


#Machine learning mastery - Non-Linear Classification in R with Decision Trees
#Cart - C4.5 - Bagging Cart - Random forest - Boosted C5.0 and more
# C4.5
library(RWeka)
data(iris)
fit <- J48(Species~., data=iris) #C4.5 model
fit
summary(fit)
plot(fit)
predictions <- predict(fit, iris[,1:4])
table(predictions, iris$Species)

#Datacamp - R Decision Trees Tutorial 
#Classification trees - Random Forests - Boosting

