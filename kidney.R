##################### Section 2 Description and High level statistics #############
df = read.csv("kidney_disease.csv")

change_categorical = c("sg","al","su","rbc","pc","pcc","ba","htn","dm","cad","appet","pe","ane")
df[,change_categorical] = lapply(df[,change_categorical], as.factor)
change_numeric = c("pcv","wc","rc")
df[,change_numeric] = lapply(df[,change_numeric], as.numeric)
View(df)

summary(df[c("age","bp","bgr","bu","sc","sod","pot","hemo","pcv","wc","rc")])

summary(df[c("sg","al","su","rbc","pc","pcc","ba","htn","dm","cad","appet","pe","ane")])

hist(df$age,
     main = "Histogram of patients' ages",
     xlab = "Age")
hist(df$bp,
     main = "Histogram of patients' blood pressure",
     xlab = "Blood pressure")
hist(df$bgr,
     main = "Histogram of patients' blood glucose random",
     xlab = "Blood glucose random")
hist(df$bu,
     main = "Histogram of patients' blood urea",
     xlab = "Blood urea")
hist(df$sc,
     main = "Histogram of patients' serum creatinine",
     xlab = "Serum creatinine")
hist(df$sod,
     main = "Histogram of patients' sodium",
     xlab = "Sodium")
hist(df$pot,
     main = "Histogram of patients' potassium",
     xlab = "Potassium")
hist(df$hemo,
     main = "Histogram of patients' hemoglobin",
     xlab = "Hemoglobin")
hist(df$pcv,
     main = "Histogram of patients' packed cell volume",
     xlab = "Packed cell volume")
hist(df$wc,
     main = "Histogram of patients' white blood cell count",
     xlab = "White blood cell count")
hist(df$rc,
     main = "Histogram of patients' red blood cell count",
     xlab = "Red blood cell count")

prop.table(table(df$sg))
prop.table(table(df$al))
prop.table(table(df$su))
prop.table(table(df$rbc))
prop.table(table(df$pc))
prop.table(table(df$pcc))
prop.table(table(df$ba))
prop.table(table(df$htn))
prop.table(table(df$dm))
prop.table(table(df$cad))
prop.table(table(df$appet))
prop.table(table(df$pe))
prop.table(table(df$ane))




############################### Section 3, 4, 5 ################################
#import dataset 
dataset <- read.csv("kidney_disease.csv")
View(dataset)
summary(dataset)

str(dataset)

dataset <- dataset[,-c(1)]

#check noisy data 
table(dataset$age)
table(dataset$bp)
table(dataset$sg)
table(dataset$al)
table(dataset$su)
table(dataset$rbc)
table(dataset$pc)
table(dataset$pcc)
table(dataset$ba)
table(dataset$bgr)
table(dataset$bu)
table(dataset$sc)
table(dataset$sod)
table(dataset$pot)
table(dataset$hemo)
table(dataset$pcv)
table(dataset$wc)
table(dataset$rc)
table(dataset$htn)
table(dataset$dm)
table(dataset$cad)
table(dataset$appet)
table(dataset$pe)
table(dataset$ane)
table(dataset$classification)


#remove white space
library(stringr)
dataset$pcv <- trimws(dataset$pcv, whitespace = "\t")
table(dataset$pcv)

dataset$wc <- trimws(dataset$wc, whitespace = "\t")
table(dataset$wc)

dataset$rc <- trimws(dataset$rc, whitespace = "\t")
table(dataset$rc)

dataset$dm <- trimws(dataset$dm, whitespace = "\t")
dataset$dm <- str_trim(dataset$dm)
table(dataset$dm)

dataset$cad <- trimws(dataset$cad, whitespace = "\t")
table(dataset$cad)

dataset$classification <- trimws(dataset$classification, whitespace = "\t")
table(dataset$classification)

dataset[dataset == ""] <- NA
dataset[dataset == "?"] <- NA

#outliers
boxplot(dataset$age,ylab = "age")

boxplot(dataset$bp, ylab = "bp")

boxplot(dataset$sg, ylab = "sg")

boxplot(dataset$bgr, ylab = "bgr")

boxplot(dataset$bu, ylab = "bu")

boxplot(dataset$sc, ylab = "sc")

boxplot(dataset$sod, ylab = "sod")

boxplot(dataset$pot, ylab = "pot")

boxplot(dataset$hemo, ylab = "hemo")

dataset$pcv <- as.numeric(dataset$pcv)
boxplot(dataset$pcv, ylab = "pcv")

dataset$wc <- as.numeric(dataset$wc)
boxplot(dataset$wc, ylab = "wc")

dataset$rc <- as.numeric(dataset$rc)
boxplot(dataset$rc, ylab = "rc")

#deal with outliers 
Q1 <- quantile(dataset$age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$age, 0.75, na.rm = TRUE)
dataset$age <- ifelse(dataset$age<Q1, Q1, ifelse(dataset$age>Q3, Q3, dataset$age))

Q1 <- quantile(dataset$bp, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$bp, 0.75, na.rm = TRUE)
dataset$bp <- ifelse(dataset$bp<Q1, Q1, ifelse(dataset$bp>Q3, Q3, dataset$bp))

Q1 <- quantile(dataset$bgr, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$bgr, 0.75, na.rm = TRUE)
dataset$bgr <- ifelse(dataset$bgr<Q1, Q1, ifelse(dataset$bgr>Q3, Q3, dataset$bgr))

Q1 <- quantile(dataset$bu, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$bu, 0.75, na.rm = TRUE)
dataset$bu <- ifelse(dataset$bu<Q1, Q1, ifelse(dataset$bu>Q3, Q3, dataset$bu))

Q1 <- quantile(dataset$sc, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$sc, 0.75, na.rm = TRUE)
dataset$sc <- ifelse(dataset$sc<Q1, Q1, ifelse(dataset$sc>Q3, Q3, dataset$sc))

Q1 <- quantile(dataset$sod, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$sod, 0.75, na.rm = TRUE)
dataset$sod <- ifelse(dataset$sod<Q1, Q1, ifelse(dataset$sod>Q3, Q3, dataset$sod))

Q1 <- quantile(dataset$pot, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$pot, 0.75, na.rm = TRUE)
dataset$pot <- ifelse(dataset$pot<Q1, Q1, ifelse(dataset$pot>Q3, Q3, dataset$pot))

Q1 <- quantile(dataset$hemo, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$hemo, 0.75, na.rm = TRUE)
dataset$hemo <- ifelse(dataset$hemo<Q1, Q1, ifelse(dataset$hemo>Q3, Q3, dataset$hemo))

Q1 <- quantile(dataset$pcv, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$pcv, 0.75, na.rm = TRUE)
dataset$pcv <- ifelse(dataset$pcv<Q1, Q1, ifelse(dataset$pcv>Q3, Q3, dataset$pcv))

Q1 <- quantile(dataset$wc, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$wc, 0.75, na.rm = TRUE)
dataset$wc <- ifelse(dataset$wc<Q1, Q1, ifelse(dataset$wc>Q3, Q3, dataset$wc))

Q1 <- quantile(dataset$rc, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$rc, 0.75, na.rm = TRUE)
dataset$rc <- ifelse(dataset$rc<Q1, Q1, ifelse(dataset$rc>Q3, Q3, dataset$rc))

#deal with missing value 
#age bp bgr bu  sc sod pot hemo pcv   wc  rc - numerical variable replace with mean
#sg al su pcc ba htn dm cad appet pe ane classification - categorical replace with mode 
#pc rbc - too much missing value, replace with unknown 

sapply(dataset, function(x) sum(is.na(x)))
dataset$age <- ifelse(is.na(dataset$age),
                     ave(dataset$age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$age)
dataset$sg <- ifelse(is.na(dataset$sg), 1.02, dataset$sg)
dataset$al <- ifelse(is.na(dataset$al), 0, dataset$al)
dataset$su <- ifelse(is.na(dataset$su), 0, dataset$su)
dataset$rbc <- ifelse(is.na(dataset$rbc), "unknown", dataset$rbc)
dataset$pc <- ifelse(is.na(dataset$pc), "unknown", dataset$pc)
dataset$pcc <- ifelse(is.na(dataset$pcc), "notpresent", dataset$pcc)
dataset$ba <- ifelse(is.na(dataset$ba), "notpresent", dataset$ba)
dataset$bp <- ifelse(is.na(dataset$bp), 
                     ave(dataset$bp, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$bp)
dataset$bgr <- ifelse(is.na(dataset$bgr),
                     ave(dataset$bgr, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$bgr)
dataset$bu <- ifelse(is.na(dataset$bu),
                    ave(dataset$bu, FUN = function(x) mean(x, na.rm = TRUE)),
                    dataset$bu)
dataset$sc <- ifelse(is.na(dataset$sc),
                    ave(dataset$sc, FUN = function(x) mean(x, na.rm = TRUE)),
                    dataset$sc)
dataset$sod <- ifelse(is.na(dataset$sod),
                     ave(dataset$sod, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$sod)
dataset$pot <- ifelse(is.na(dataset$pot),
                     ave(dataset$pot, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$pot)
dataset$pcv <- ifelse(is.na(dataset$pcv),
                     ave(dataset$pcv, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$pcv)
dataset$wc <- ifelse(is.na(dataset$wc),
                    ave(dataset$wc, FUN = function(x) mean(x, na.rm = TRUE)),
                    dataset$wc)
dataset$hemo <- ifelse(is.na(dataset$hemo),
                      ave(dataset$hemo, FUN = function(x) mean(x, na.rm = TRUE)),
                      dataset$hemo)
dataset$rc <- ifelse(is.na(dataset$rc),
                    ave(dataset$rc, FUN = function(x) mean(x, na.rm = TRUE)),
                    dataset$rc)
dataset$htn <- ifelse(is.na(dataset$htn), "no", dataset$htn)
dataset$dm <- ifelse(is.na(dataset$dm), "no", dataset$dm)
dataset$cad <- ifelse(is.na(dataset$cad), "no", dataset$cad)
dataset$appet <- ifelse(is.na(dataset$appet), "good", dataset$appet)
dataset$pe <- ifelse(is.na(dataset$pe), "no", dataset$pe)
dataset$ane <- ifelse(is.na(dataset$ane), "no", dataset$ane)

#encoding 
dataset$sg <- factor(dataset$sg, levels = c(1.005, 1.01, 1.015, 1.02, 1.025))
dataset$al <- factor(dataset$al, levels = c(0, 1, 2, 3, 4, 5))
dataset$su <- factor(dataset$su, levels = c(0, 1, 2, 3, 4, 5))
dataset$rbc <- factor(dataset$rbc, levels = c("abnormal", "normal", "unknown"), labels = c(0, 1, 2))
dataset$pc <- factor(dataset$pc, levels = c("abnormal", "normal", "unknown"),  labels = c(0, 1, 2))
dataset$pcc <- factor(dataset$pcc, levels = c("notpresent", "present"), labels = c(0, 1))
dataset$ba <- factor(dataset$ba, levels = c("notpresent", "present"), labels = c(0, 1))
dataset$htn <- factor(dataset$htn, levels = c("no", "yes"), labels = c(0, 1))
dataset$dm <- factor(dataset$dm, levels = c("no", "yes"), labels = c(0, 1))
dataset$cad <- factor(dataset$cad, levels = c("no", "yes"), labels = c(0, 1))
dataset$appet <- factor(dataset$appet, levels = c("good", "poor"), labels = c(0, 1))
dataset$pe <- factor(dataset$pe, levels = c("no", "yes"), labels = c(0, 1))
dataset$ane <- factor(dataset$ane, levels = c("no", "yes"), labels = c(0, 1))
dataset$classification <- factor(dataset$classification, levels = c("notckd", "ckd"), labels = c(0, 1))

#split dataset
library(caTools)
set.seed(1)
split <- sample.split(dataset, SplitRatio = 0.7) 
training_set <- subset(dataset, split == "TRUE") 
test_set <- subset(dataset, split == "FALSE") 

#scaling
index <- sapply(training_set, is.numeric)
training_set[index] <- lapply(training_set[index], scale)
test_set[index] <- lapply(test_set[index], scale)


#build naive bayes, decision tree and knn
#naive Bayes
library(e1071)
classifier <- naiveBayes(classification~., data = training_set)
classifier

#predicting the test_set results
nb_predict <- predict(classifier, test_set, type = "class")

#Confusion Matrix of Naive Bayes
library('caret')
confusionMatrix(data=nb_predict, 
                reference=test_set$classification)

#decision tree classification
library(rpart.plot)
decision_tree <- rpart(classification~., data = training_set, method = 'class')
rpart.plot(decision_tree)

#Predict the test_set results
dt_predict <- predict(decision_tree, test_set, type="class")

#Confusion Matrix of Decision tree
confusionMatrix(data=dt_predict, 
                reference=test_set$classification)


#knn classification
library("class")

idx <- sapply(dataset, is.factor)
training_set[idx] <- lapply(training_set[idx], function(x) as.numeric(as.character(x)))
test_set[idx] <- lapply(test_set[idx], function(x) as.numeric(as.character(x)))

a <- sqrt(NROW(training_set))
knn <- knn(train=training_set, test=test_set, cl=training_set$classification, k=a)
knn

#Confusion Matrix of KNN
confusionMatrix(table(knn,test_set$classification))
