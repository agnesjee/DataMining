
setwd("C:/DATA ARTHA/KULIAH TAYLORS UNIVERSITY/Data Mining/Practical/Lab 12")

# tidyverse for data manipulation and visualization
library(tidyverse)
data("marketing", package = "datarium")
head(marketing, 4)
print(marketing)
# model<-lm(sales~youtube,data = marketing)
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)

summary(model)

summary(model)$coefficient


model <- lm(sales ~ youtube + facebook, data = marketing)
summary(model)

confint(model)

sigma(model)/mean(marketing$sales)


model <- lm(sales ~., data = marketing)
summary(model)

model <- lm(sales ~. -newspaper, data = marketing)
summary(model)

model1 <- update(model, ~. -newspaper)
summary(model1)

