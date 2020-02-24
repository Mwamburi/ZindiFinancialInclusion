#You are asked to make predictions for each unique id in the test dataset about the likelihood of 
#the person having a bank account.
#You will train your model on 70% of the data and test your model on the final 30% of the data


#import libraries 
library(dplyr)
library(readr)
library(C50)
library(caret)
library(gmodels)


#import train data
train <- read_csv("Train_v2.csv")
train <-as.data.frame(train)

#import test data

test <- read_csv("Test_v2.csv")
test <- as.data.frame(test)

#Trasnsform variables into factors


cols <- c("country",               
          "uniqueid",               
          "bank_account",           
          "location_type",         
          "cellphone_access",
          "gender_of_respondent",
          "relationship_with_head",
          "marital_status",
          "job_type",
          "education_level")
train[cols] <- lapply(train[cols], factor)
test[cols] <- lapply(test[cols], factor)

#split training set via stratified sampling into a validation set as the test set
#contains no target variable.names
  
in_train <- createDataPartition(train$bank_account,p = 0.75,list = FALSE)
credit_train <- train[in_train,]
credit_test <- train[-in_train,]


#create model
credit_model <- C5.0(credit_train[-4],credit_train$bank_account)
credit_model
summary(credit_model)

#Prediction
credit_predict_1 <-predict(credit_model,credit_test) 

#Evaluation

CrossTable(credit_test$bank_account,credit_predict_1,
           prop.r = TRUE,prop.c = FALSE,prop.chisq = FALSE,
           dnn = c("Actual","Predicted"))

#We have a poor precision but rather competitive recall. 
#Time to boost!

#Boosting through iteration

credit_booost10 <- C5.0(train[-4],train$bank_account, trials = 10)
summary(credit_booost10)

#further optimization using caret



