# Importing the dataset
# print(getwd())
# setwd("C:/MyFiles/The Taylor's University/TeachingClass/Teaching Subjects/Data Mining/March 2020/4. Teaching Materials/Lab/Lab 5")

dataset = read.csv('Social_Network_Ads.csv')

dataset = dataset[3:5]
print(dataset)
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123) #learning SetSeed function: https://rpubs.com/Mentors_Ubiqum/Set_Seed  to learn about SetSeed function and key concepts like random number generation with R.

split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# print(test_set[-3])
# Feature Scaling
training_set[-3] = scale(training_set[-3])  #scale function: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/scale
test_set[-3] = scale(test_set[-3])
# print(test_set[-3])

# Fitting naivebayes to the Training set
# install.packages('e1071')  #learn more about the package: https://data-flair.training/blogs/e1071-in-r/
library(e1071)
#learn more about naivebayes function in R: https://www.rdocumentation.org/packages/e1071/versions/1.7-3/topics/naiveBayes
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix #learn more about Confusion Matrix: https://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)

set = training_set

X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2) # Learn more about expand.grid: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/expand.grid

colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Naivebayes (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Naivebayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
