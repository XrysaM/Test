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
fit <- rpart(survived~., data = data_train, method = 'class') #method= which algorithm
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
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)
rpart.plot(tune_fit, extra = 106)

accuracy_tune(tune_fit)

