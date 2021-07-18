print(getwd())
# setwd("C:/MyFiles/The Taylor's University/TeachingClass/Teaching Subjects/Data Mining/March 2020/4. Teaching Materials/Lab/Lab 9/Code and Dataset")
setwd("C:/DATA ARTHA/KULIAH TAYLORS UNIVERSITY/Data Mining/Practical/Lab 9/Code and Dataset")
print(getwd())

# Hierarchical Clustering

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]
print(dataset)

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

