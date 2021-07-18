# Get and print current working directory.
print(getwd())

#Reading a CSV File
dataset <- read.csv ("Datasets.csv")
print(dataset)

#Analyzing the CSV File
print(is.data.frame(dataset))
print(ncol(dataset))
print(nrow(dataset))

# Get the max salary from data frame. We will see error as we have NA value in the salary column
sal <- max(dataset$Salary)
print(sal)

# crude ways
is.na(dataset$Salary)
is.na(dataset$Age)

dataset_salary <- dataset['Salary']
dataset_salary
dataset_salary<-dataset_salary[!is.na(dataset_salary)]
dataset_salary

mean_salary<-mean(dataset_salary)
mean_salary

dataset$Salary[5]

dataset$Salary[5]<-mean_salary

dataset

# more elegance ways
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)
print(dataset)

dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
print(dataset)

dataset$Age = floor(dataset$Age)
print(dataset)


# Encoding categorical data
dataset$Country = factor(dataset$Country,
                         levels = c('Indonesia', 'Malaysia', 'Singapore'),
                         labels = c(1, 2, 3))
print(dataset)

dataset$Purchased = factor(dataset$Purchased,
                          levels = c('No', 'Yes'),
                          labels = c(0, 1))
print(dataset)

#The latest version of caTools requieres R >= 3.6.0, so you have to update R first in order to been able to install it.
#install/update R https://uvastatlab.github.io/phdplus/installR.html
# check the version R.version.string       the result shows that version 3.5.2 (“Eggshell Igloo”) is currently installed

# To split the dataset
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.5) #DependentVariable in our case is Purchased variable
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
set.seed(123)
training_set [, 2:3]
test_set [, 2:3]
training_scaled_set = scale (training_set [, 2:3] )
test_scaled_set = scale (test_set [, 2:3] )

print(Training_set)

print(Test_set)


training_scaled_set = scale (training_set [, 2:3], center = FALSE)
test_scaled_set = scale (test_set [, 2:3] ,center = FALSE)

print(training_scaled_set)

print(test_scaled_set)
