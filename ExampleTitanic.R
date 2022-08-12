#This is an example [titanic]
#Decision Tree in R: Classification Tree with Example 
#https://www.guru99.com/r-decision-trees.html
#The purpose of this dataset is to predict which people are more likely to survive after the collision with the iceberg. 
#The dataset contains 13 variables and 1309 observations. 
#The dataset is ordered by the variable X.

#load dataset
set.seed(678)
path <-'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
#shuffle gia na paroume times apo ola ta classes 
shuffle_index <- sample(1:nrow(titanic)) #rand list, length=rows
titanic <- titanic[shuffle_index, ] 

#clean dataset
library(dplyr)
clean_titanic <- titanic %>%
  #drop unnecessary variables
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  #make values readable (1,2 -> No,Yes)
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes')),
         age = as.numeric(age), #age,fare are characters(has ?/NAs) - fixed na.omit and plot
         fare = as.numeric(fare)) %>%
  na.omit() #remove NA observations

#Train/test set
#make function(data set, split size, train (y/n))
#train set (80/20)
create_train_test <- function(data, size = 0.8, train = TRUE) #set defaults
{
  n_row = nrow(data)
  total_row = size * n_row #0.8*data number_of_rows = mexri auton ton ari8mo einai to 80%
  train_sample <- 1: total_row #train= apo 1 mexri to nth ari8mo
  if (train == TRUE) {
    return (data[train_sample, ]) #rows= 1:total_row, cols=all)
  } else {
    return (data[-train_sample, ])#rows= the rest(not them)
  }
}
#call function - make train, test set
data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE) #shouldn't it be 0.2? oxi giati pairnoume to 0.2 me to "train"(auto pou menei einai to 20%)
dim(clean_titanic)
dim(data_train)
dim(data_test)
#check randomization (if both results are roughly the same) frequency table (%)
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

#Rpart decision tree - train & tree
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(survived~., 
             data = data_train, 
             method = 'class') #anova/poisson/class/exp
rpart.plot (fit, extra = 106)

#Test & prediction
#predict_unseen <-predict(fit, data_test, type = 'class')
#table_mat <- table(data_test$survived, predict_unseen)
#table_mat

#accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
#print(paste('Accuracy for test', accuracy_Test))

#tuning parameters
#same as before but in function
accuracy_tune <- function(fit) #giati parameter to fit mono? gt oxi to data test?
{
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  return(list(table_mat,paste('Accuracy for test', accuracy_Test)))
}
accuracy_tune(fit)

#dokimazw times na dw an kalutereuei to model
control <- rpart.control(minsplit = 10, #min obs before split
                         #minbucket = round(5 / 3), # min obs in the leaf
                         maxdepth = 3, 
                         cp = 0)
tune_fit <- rpart(survived~., 
                  data = data_train, 
                  method = 'class', 
                  control = control)
rpart.plot(tune_fit, extra = 106)

accuracy_tune(tune_fit)




#using random forest for parameter tuning
#https://www.guru99.com/r-random-forest-tutorial.html

library(randomForest)
library(caret)
library(e1071)

# Define the control
trControl <- trainControl(method = "cv", #Cross Validation
                          number = 10,   #folds (usually 5 or 10)
                          search = "grid") #grid / random
set.seed(1234)
# train model
rf_default <- train(survived~.,
                    data = data_train,
                    method = "rf",   #which classification (or regression) model 
                    metric = "Accuracy",
                    trControl = trControl)
rf_default
#find best mtry
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(3: 10))
rf_mtry <- train(survived~.,
                 data = data_train,
                 method = "rf",
                 metric = "Accuracy", #Accuracy/Kappa for classification
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
rf_mtry
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry
#find best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(20 : 30)) {
  set.seed(1234)
  rf_maxnode <- train(survived~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode #list pou se ka8e 8esh(20-30) exei to antistoixo train 
}
results_mtry <- resamples(store_maxnode) 
summary(results_mtry)
  #find max accuracy
df_nodes <-results_mtry$values[seq(2,22,by=2)]
max(df_nodes)
  #what columns have the max value
names(results_mtry$values)[which(results_mtry$values == max(df_nodes), arr.ind=T)[, "col"]]
#deixnei pws epilegei to mikrotero max value, se emas einai to 25 (20-30)

#find best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(survived~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 25,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

df_tree <- results_tree$values[seq(2,22,by=2)]
max(colMeans(df_tree))

dfmeans <- data.frame(colMeans(df_tree)) #8etw dataframe kai xrhsimopoiw auto ws pinaka(df) anaforas
rownames(dfmeans)[which(dfmeans == max(colMeans(df_tree)), arr.ind=T)[, 1]]
#deixnei pws epilegei to megalutero mean value, se emas einai to 400

#use what we have found
fit_rf <- train(survived~.,
                data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 400,
                maxnodes = 25)
#evaluate model
prediction <-predict(fit_rf, data_test)
confusionMatrix(prediction, data_test$survived)
varImp(fit_rf)
plot(fit_rf$finalModel)

#dokimh me randomForest function
fit_rf2 <- randomForest(survived~.,
                data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 400,
                maxnodes = 25)

prediction <-predict(fit_rf2, data_test)
confusionMatrix(prediction, data_test$survived)
varImpPlot(fit_rf2)
#auta bgazoun diaforetika apotelesmata
varImp(fit_rf2)
plot(fit_rf2) 