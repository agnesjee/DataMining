# Name: Agnes Jee Chian Hwa (0339809)

# Make sure in the correct working directory
print(getwd())
setwd("C:/Users/ASUS/Downloads")

# Load the dataset
dataset <- read.csv("premium.csv")
View(dataset)
summary(dataset)
str(dataset)

# Load library packages
library(dplyr)
library(tidyverse)
library(ggplot2)

# Check noisy data
table(dataset$age)
table(dataset$sex)
table(dataset$bmi)
table(dataset$children)
table(dataset$smoker)
table(dataset$region)
table(dataset$premium)

# Check outliers on numerical variables
boxplot(dataset$age, ylab="age")
boxplot(dataset$bmi, ylab="bmi")
boxplot(dataset$children, ylab="children")
boxplot(dataset$premium, ylab="premium")

# Check on missing values
colSums(is.na(dataset))

# Deal with missing values
dataset$bmi = ifelse(is.na(dataset$bmi),
                     ave(dataset$bmi, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$bmi)

dataset$children = ifelse(is.na(dataset$children),
                          ave(dataset$children, FUN = function(x) mean(x, na.rm = TRUE)),
                          dataset$children)

colSums(is.na(dataset))

summary(dataset)
str(dataset)

### Exploratory Data Analysis ###
# install.packages("cowplot")
library(cowplot)

# Correlation between premium and age / bmi
x <- ggplot(dataset, aes(age, premium)) +
  geom_jitter(color = "blue", alpha = 0.5) + 
  theme_light()

y <- ggplot(dataset, aes(bmi, premium)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y)
title <- ggdraw() +draw_label("1. Correlation between Premium and Age / BMI", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))

# Correlation between premium and sex / children
x <- ggplot(dataset, aes(sex, premium)) +
  geom_jitter(aes(color = sex), alpha = 0.7) + 
  theme_light()

y <- ggplot(dataset, aes(children, premium)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y)
title <- ggdraw() +draw_label("2. Correlation between Premium and Sex / Children", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))

# Correlation between premium and smoker / region
x <- ggplot(dataset, aes(smoker, premium)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) + 
  theme_light()

y <- ggplot(dataset, aes(region, premium)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y)
title <- ggdraw() +draw_label("3. Correlation between Premium and Smoker / Region", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))


### Data Preprocessing ###

# Encode sex, smoker and region
str(dataset)
dataset$sex = factor(dataset$sex, 
                     levels = c('male', 'female'),
                     labels = c(1, 2))

dataset$smoker = factor(dataset$smoker, 
                        levels = c('no', 'yes'),
                        labels = c(0, 1))

dataset$region = factor(dataset$region,
                        levels = c('northeast', 'northwest', 'southeast', 'southwest'),
                        labels = c(1, 2, 3, 4))

str(dataset)

# Check the collinearity test between numeric features
# install.packages("psych")
library(psych)
pairs.panels(dataset[c("age", "bmi", "children", "premium")])

# Find correlation among numeric features in table form
library(corrplot)
cor(dataset[sapply(dataset, is.numeric)])

# Splitting dataset into training and testing dataset
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

### Model Building ###
library(caret)

# k-fold cross validation
control <- trainControl(method = "repeatedcv", repeats = 10)

# Decision Tree
# Fit Decision Tree model to the training set
dt_model = train(premium~., 
                  data = training_set, 
                  trControl = control, 
                  method = "rpart")

# Summary of the training model
summary(dt_model)

# Predict the test set results
y_pred_dt = predict(dt_model, newdata = test_set)

# Applying on new data
Smith <- data.frame(age = 20,
                    sex = "1", 
                    bmi = 28, 
                    children = 0,
                    smoker = "1", 
                    region = "3")

print(paste0("Health insurance premium for Smith: ",
             round(predict(dt_model, Smith), 2)))

# Calculate RMSE of Decision Tree
rmse_dt = RMSE(test_set$premium, y_pred_dt)
cat("Root Mean Square Error for Decision Tree Model: ", rmse_dt)


# Multiple Linear Regression
# Fit Multiple Linear Regression to the training set
mlr_model = train(premium~., 
                  data = training_set, 
                  trControl = control,
                  method="lm")

# Summary of the training model
summary(mlr_model)

# Predict the test set results
y_predict_mlr = predict(mlr_model, newdata = test_set)

# Applying on new data
Smith <- data.frame(age = 20,
                    sex = "1", 
                    bmi = 28, 
                    children = 0,
                    smoker = "1", 
                    region = "3")

print(paste0("Health insurance premium for Smith: ",
             round(predict(mlr_model, Smith), 2)))

# Calculate RMSE of Multiple Linear Regression - first version
rmse_mlr = RMSE(test_set$premium, y_predict_mlr)
cat("Root Mean Squared Error for Multiple Linear Regression Model: ",
    rmse_mlr)




