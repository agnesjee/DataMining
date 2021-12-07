# Name: Agnes Jee Chian Hwa
# Student ID: 0339809

# To get and print current working directory
print(getwd())

# Change to desirable working directory
setwd("C:/Users/ASUS/Downloads")
print(getwd())

# load the dataset
mydf <- read.csv("telco-churn.csv")

# load library packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)

# To look at the dimensions of mydf
dim(mydf)

# To preview the data
head(mydf)

# Creating a new tbl_df called cran
cran <- tbl_df(mydf)

# Remove the original data frame to avoid confusion
rm("mydf")

# Print out in tbl_df would be much more informative
cran

# View the structure of the dataset
str(cran)

# Convert those with character type to factor, and type of "SeniorCitizen" into factor as well
cran <- cran %>%
  mutate_if(is.character, as.factor)

cran$SeniorCitizen <- as.factor(cran$SeniorCitizen)

# Recode the attributes values to appriopriate ones
cran <- cran %>% 
  mutate(SeniorCitizen=recode(SeniorCitizen,
                                     "0"="No",
                                     "1"="Yes"))

cran <- cran %>%
  mutate(MultipleLines=recode(MultipleLines,
                              "No phone service"="No"))

cran <- cran %>%
  mutate(OnlineSecurity=recode(OnlineSecurity,
                              "No internet service"="No"))

cran <- cran %>%
  mutate(OnlineBackup=recode(OnlineBackup,
                              "No internet service"="No"))

cran <- cran %>%
  mutate(DeviceProtection=recode(DeviceProtection,
                              "No internet service"="No"))

cran <- cran %>%
  mutate(TechSupport=recode(TechSupport,
                              "No internet service"="No"))

cran <- cran %>%
  mutate(StreamingTV=recode(StreamingTV,
                              "No internet service"="No"))

cran <- cran %>%
  mutate(StreamingMovies=recode(StreamingMovies,
                              "No internet service"="No"))

# Grouping of tenure
min(cran$tenure)
max(cran$tenure)

group_tenure <- function(tenure){
  if(tenure >= 0 & tenure <= 12){
    return ("0-12 Month")
  }else if(tenure > 12 & tenure <= 24){
    return ("12-24 Month")
  }else if(tenure > 24 & tenure <= 48){
    return ("24-48 Month")
  }else if(tenure > 48 & tenure <= 60){
    return ("48-60 Month")
  }else if(tenure > 60){
    return ("> 60 Month")
  }
}

cran$tenure_group <- sapply(cran$tenure, group_tenure)
cran$tenure_group <- as.factor(cran$tenure_group)

# Remove customerID and tenure
cran$customerID <-NULL
cran$tenure <- NULL

str(cran)

# Check missing values
sum(is.na(cran))
sapply(cran, function(x) sum(is.na(x)))

# Change missing values in "TotalCharges" with mean values
cran$TotalCharges = ifelse(is.na(cran$TotalCharges), 
                           ave(cran$TotalCharges, FUN = function(x) mean(x, na.rm = TRUE)),
                           cran$TotalCharges)
sapply(cran, function(x) sum(is.na(x)))

# check again with the dataset
str(cran)

# Correlation between numeric variables
numeric.var <- sapply(cran, is.numeric)
corr.matrix <- cor(cran[, numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# Remove one of the numeric variables because they are correlated
cran$TotalCharges <- NULL

# Split dataset into training and test set
library(caTools)
set.seed(100)
split = sample.split(cran$Churn, SplitRatio = 0.75)
training_set = subset(cran, split == TRUE)
testing_set = subset(cran, split == FALSE)

# Decision Tree

# Option 1: ctree
library(partykit)
model_dt <- ctree(formula = Churn ~., 
                  data = training_set)

plot(model_dt)

# Prediction with decision tree model
ctree.predict <- predict(model_dt, testing_set)
table(ctree.predict, testing_set$Churn)

# Creating confusion matrix for testing set

# install.packages("caret")
# install.packages("e1071", dependecies = TRUE)
library(caret)

# testing set
confusionMatrix(data=ctree.predict, 
                      reference=testing_set$Churn, 
                      positive="Yes")

# Option 2: rpart
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

fit <- rpart(Churn~., data = training_set, method = "class")
rpart.plot(fit, extra = 106)

# Predict the test dataset
rpart_predict <- predict(fit, testing_set, type = "class")

# View the classification table of testing set
table(testing_set$Churn, rpart_predict)

# testing set
confusionMatrix(data = rpart_predict,
                reference = testing_set$Churn,
                positive = "Yes")

# Naive Bayes
library(e1071)
model_nb = naiveBayes(training_set[,!names(training_set) %in% c("Churn")],
                      training_set$Churn)

# To examine the function call, a-priori & conditional probability
model_nb

# Predicting the testing set results
nb_predict <- predict(model_nb, testing_set, type="class")

# Generate classification table for the testing dataset
table(testing_set$Churn, nb_predict)

# Confusion Matrix of testing set
confusionMatrix(data = nb_predict, 
                reference = testing_set$Churn,
                positive = "Yes")

# Variable Importance
library(caret)
# rpart variable importance
importance = varImp(fit, scale = FALSE)
importance
