## Decision Tree


## About the data: 
## Let’s consider a Company dataset with around 10 variables and 400 records. 
## The attributes are as follows: 
## Sales -- Unit sales (in thousands) at each location
## Competitor Price -- Price charged by competitor at each location
## Income -- Community income level (in thousands of dollars)
## Advertising -- Local advertising budget for company at each location (in thousands of dollars)
## Population -- Population size in region (in thousands)
## Price -- Price company charges for car seats at each site
## Shelf Location at stores -- A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
## Age -- Average age of the local population
## Education -- Education level at each location
## Urban -- A factor with levels No and Yes to indicate whether the store is in an urban or rural location
## US -- A factor with levels No and Yes to indicate whether the store is in the US or not
##The company dataset looks like this:

## Problem Statement:
##A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
##Approach - A decision tree can be built with target variable Sale (we will first convert it in categorical variable) & all other variable will be independent in the analysis.  


company_data <- read.csv(file.choose())
summary(company_data)
str(company_data)

sum(is.na(company_data)) ## no missing values

library(moments)
hist(company_data$Sales)
boxplot(company_data$Sales) ### its showing an outlier

## now we will convert sales in categorical variable

sales_cat <- ifelse(company_data$Sales < 10,"No","Yes")
data_company <- data.frame(company_data,sales_cat) ## adding sales_cat to original data and creating new data set

## now will split data into train and test 

company_data_train <- data_company[1:200,]
company_data_test <- data_company[201:400,]

## lets build our first model using tree function

library(tree)

attach(data_company)

company_data_model <- tree(sales_cat ~ . -Sales,data = data_company)
summary(company_data_model)
## we got Number of terminal nodes:  21 

plot(company_data_model) ## plot tree for our first model
text(company_data_model,pretty = 0) ## lets label the tree we plot

## prop.table(table(data_company$Sales))

## now we will build our model on training data set
attach(company_data_train)

company_train_model <- tree(sales_cat ~ .- Sales,data = company_data_train)
summary(company_train_model) ## Number of terminal nodes:  12 

plot(company_train_model)
text(company_train_model,pretty = 0)

## from tree we can say that if price < 96.5 and advertising < 7.5 then
##companyPrice< 119.5 it will help to increase the sale


## lets evaluate the performance of model

company_pred <- as.data.frame(predict(company_train_model,newdata = company_data_test))
company_pred["final"] <- NULL

company_pred_test <- predict(company_train_model,newdata = company_data_test)

## allocate sale_cat to our predicted values and create new final column

company_pred$final <- colnames(company_pred_test)[apply(company_pred_test,1,which.max)]
company_pred$final <- as.factor(company_pred$final)

summary(company_pred$final) ### these are predicted values
# No  Yes 
# 172  28 

summary(company_data_test$sales_cat) ## these are our original values
# No Yes 
# 162  38 

mean(company_pred$final == data_company$sales_cat) ## accuracy of model is 0.7725

## lets plot confusion matrix

table(company_pred$final,company_data_test$sales_cat)
##        No  Yes
##   No  153   19
##  Yes   9    19