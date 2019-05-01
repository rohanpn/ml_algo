#install.packages("caret")
#install.packages('dplyr')
#install.packages("rpart") #rpart is partitioning of decision tree
#install.packages("rpart.plot") #rpart creates the tree whch will be plotted
#install.packages("data.tree")#use to visualize the data tree
#install.packages("caTools")#used for manipulation of data and split function
#install.packages("ElemStatLearn")#the purpose of importing this package is to work on data setup in terms of statistics and data processing
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("e1071")


library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)
library(ElemStatLearn)
library(readxl)
library(e1071)

### naive bayes##########################
# collecting and exploring data
file_url = "https://data.princeton.edu/wws509/datasets/copen.dat"
dest_file = "csv_file1.csv"

set.seed(123)
read_data = read.table(file_url, header = TRUE)
dplyr::glimpse(read_data)
write.csv(read_data, dest_file)
file_data = read.csv(dest_file)
str(file_data)
head(file_data)

# test data and train data
sample = sample(2, nrow(file_data), replace = T, prob = c(0.8, 0.2))
train = file_data[sample == 1,]
test = file_data[sample == 2,]

# apply algorithm
fit <- naiveBayes(contact ~ ., data = train)
fit

#predict 
p_train = predict(fit, train)
p_test = predict(fit, test)

#confusion matrix
train_conf_martrx = table(p_train, train$contact)
test_conf_martrx = table(p_test, test$contact)




