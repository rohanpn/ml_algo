install.packages("rebmix")
install.packages("knnGarden")
library(rebmix)
library(knnGarden)

#Collecting and Exploring Data
data("wine")
str(wine)

#Preparing Data
data_sample <- wine[, 1:13]
head(data_sample)
set.seed(2016)
n <- nrow(data_sample)
#Train and Test set
train <- sample(1:n, 89, replace = FALSE)
head(train)

#Train Model using Train Set
fit1 <- knnVCN(data_sample[train,], wine$Cultivar[train], data_sample[-train,], K=2, method = "canberra")

#Evaluate Model Performance
tab1 <- table(fit1$TstXIBelong, wine$Cultivar[-train])
tab1
#ErrorRate = Incorrect/TotalObseravtionsInTrain = 3/89 = 0.334%
#Accuracy = 1 - ErrorRate = 1 - 0.334 = 96.63%

#Improving Model Performance
fit2 <- knnVCN(data_sample[train,], wine$Cultivar[train], data_sample[-train,], K=3, method = "euclidean")
tab2 <- table(fit2$TstXIBelong, wine$Cultivar[-train])
tab2
