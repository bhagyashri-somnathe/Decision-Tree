#Use decision trees to prepare a model on fraud data 
#treating those who have 
#taxable_income <= 30000 as "Risky" and others are "Good"

#Data Description :
  
#Undergrad : person is under graduated or not
#Marital.Status : marital status of a person
#Taxable.Income : Taxable income is the amount of how much tax an individual owes to the government 
#Work Experience : Work experience of an individual person
#Urban : Whether that person belongs to urban area or not


install.packages("tree")
library(tree)

fraud_data <- read.csv(file.choose())
str(fraud_data)

## check any missing value is there

sum(is.na(fraud_data)) ## there is no missing value 

## lets check plot boxplot and histogram

library(moments)
hist(fraud_data$Taxable.Income)
boxplot(fraud_data$Taxable.Income)

## here we have to classify people in two category i.e.
## 1. taxable_income <= 30000 as "Risky" and 
## 2. others are "Good"

risk_good <- ifelse(fraud_data$Taxable.Income <= 30000,"Risky","Good")

## Use ifelse to categorize it and store these into fraud_check dataframe

fraud_check <- data.frame(fraud_data,risk_good)

prop.table(table(fraud_check$risk_good)) 
## from this we can analyse that 20% of people have taxable_income <= 30000;
## Good     Risky 
## 0.7933333 0.2066667

## now we will split data into train and test

fraud_check_train <- fraud_check[1:300,]
fraud_check_test <- fraud_check[301:600,]

## lets try to build our 1st model using decision tree 
attach(fraud_check)

fraud_model <- tree(risk_good ~ .-Taxable.Income,data = fraud_check)
fraud_model

summary(fraud_model) ## its giving single node

## here we got Number of terminal nodes:  1

plot(fraud_model) # can not build model on single node

## now we will build our model on training data

attach(fraud_check_train)

fraud_train_model <- tree(risk_good ~ .- Taxable.Income,data = fraud_check_train)

summary(fraud_train_model) ## Number of terminal nodes:  12 

plot(fraud_train_model)
text(fraud_train_model,pretty = 0)

## lets do prediction and evaluate performance of model

fraud_pred <- as.data.frame(predict(fraud_train_model,newdata = fraud_check_test))
fraud_pred

fraud_pred["Final"] <- NULL

fraud_pred_dataframe <- predict(fraud_train_model,newdata = fraud_check_test)           
fraud_pred$Final <- colnames(fraud_pred_dataframe)[apply(fraud_pred_dataframe,1,which.max)]
fraud_pred$Final <- as.factor(fraud_pred$Final)

summary(fraud_pred$Final)
# Good Risky 
# 277    23 

summary(fraud_check_test$risk_good)
# Good Risky 
# 246    54 

mean(fraud_pred$Final == fraud_check_test$risk_good) ## accuracy for this model is 0.7566667

## confusion matrix 

table(fraud_pred$Final,fraud_check_test$risk_good)

##        Good Risky
## Good   225    52
## Risky   21     2
