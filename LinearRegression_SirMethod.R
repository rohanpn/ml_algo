install.packages("archdata")
install.packages("caret")
install.packages("hydroGOF")
library(archdata)
library(ggplot2)
library(caret)
library(hydroGOF)

#Collecting and Exploring Data
data("Handaxes")
str(Handaxes)

#Preparing Data
y <- log(data.matrix(Handaxes[, 2]))
data_sample <- log(data.frame(data.matrix(Handaxes[, 3:8])))
head(round(data_sample, 3)) #features
head(round(y, 3))
#Finding Correlation
cor_matrix <- cor(data.matrix(data_sample))
rid_col <- findCorrelation(cor_matrix, cutoff = 0.6, exact = FALSE)
rid_col
colnames(data_sample)[rid_col]
data_sample$B1 <- NULL
data_sample$B <- NULL
head(round(data_sample, 3))
#Train and Test set
set.seed(2016)
N <- nrow(data_sample)
train <- sample(1:N, 550, FALSE)
y_train <- y[train]
y_test <- y[-train]
data_train <- data_sample[train, ]
data_test <- data_sample[-train, ]
nrow(data_train)
nrow(data_test)

#Train Model using Train Set
fit = lm(y_train ~ ., data = data_train)
round(fit$coefficients, 3)
#Exploring the coefficient T1 (Since T1 is negative in coefficient)
fitT1 <- lm(y_train ~T1, data = data_train)
round(fitT1$coefficients, 3)
#Here T1 coeffient has expected positive sign
#We drop T1 from the model
fit <- lm(y_train ~L1+B2+T, data = data_train)
round(fit$coefficients, 3)

#Evaluate Model Performance
summary(fit)
p1 <- predict(fit, data_train)
#Calculate RMSE 
RMSE <- rmse(p1, y_train)
