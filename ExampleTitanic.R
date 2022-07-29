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
clean_titanic <- titanic % > %
  #drop unnecessary variables
  select(-c(home.dest, cabin, name, X, ticket)) % > % 
  #make values readable (1,2 -> No,Yes)
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) % > %
  na.omit() #remove NA observations

#Train/test set
#make function(dataset, splitsize, train (y/n))
#train set (80/20)
create_train_test <- function(data, size = 0.8, train = TRUE) #set defaults
{
  n_row = nrow(data)
  total_row = size * n_row #0.8*data number_of_rows = mexri auton ton ari8mo einai to 80%
  train_sample < - 1: total_row #train= apo 1 mexri to nth ari8mo
  if (train == TRUE) {
    return (data[train_sample, ]) #rows= 1:total_row, cols=all)
  } else {
    return (data[-train_sample, ])#rows= the rest(not them)
  }
}
#call function - make train, test set
data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)#shouldn't it be 0.2?
dim(data_train)#dimension?
dim(data_test)


