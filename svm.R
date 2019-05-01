#install.packages('mlbench')
library(mlbench)
data("PimaIndiansDiabetes2")
ncol(PimaIndiansDiabetes2)

nrow(PimaIndiansDiabetes2)


str(PimaIndiansDiabetes2)


##### Dealing with NA values

sapply(PimaIndiansDiabetes2,function(x)sum(is.na(x)))
data <- (PimaIndiansDiabetes2)

#### remove the some attributes who have more NA values
data$insulin <- NULL
data$triceps <- NULL

data <- na.omit(data)

nrow(data)

ncol(data)


####### convert charachtersitics to some numeric

y <- (data$diabetes)
levels(y) <- c("-1","1") # neg and pos replace with '-1' and '+1'
y<- as.numeric(as.character(y))
y<- as.matrix(y)

### Scaling the input attribute

x <- data
x$diabetes <- NULL
x<- as.matrix(x)
x<-scale(x) ## for scale every feature should me in  numeric

#### select the train and test sets

set.seed(103)
n=nrow(x)
train <- sample (1:n,600,FALSE)

### Train model using train set
i#nstall.packages('svmpath')
require(svmpath)
fit <- svmpath(x[train,],y[train,],kernel.function = radial.kernel,trace=TRUE) 


head(fit$lambda,3)

head(fit$Error,3)


with(fit, Error[Error == min[Error]])


###### selecting the smallest cost parameter from the min no of errors

error<- with(fit,Error[Error == min(Error)])

min_err_row <- which(fit$Error == min(fit$Error))

temp_lambda <- fit$lambda[min_err_row]

loc <- which(fit$lambda[min_err_row] == min(fit$lambda[min_err_row]))

lambda <- temp_lambda[loc]
lambda


### making prediction

pred_train <- predict(fit,newx = x[train,],lambda = lambda,type = "class")

table(y[train,],pred_train,dnn = c("Observed","Predicted"))


### Evaluate model

pred_test <- predict(fit,newx = x[-train,],lambda = lambda,type="class")

table(y[-train,],pred_test,dnn = c("Observed","Predicted"))


### Improve the model performance

fitP <- svmpath(x[train,],y[train,],kernel.function = poly.kernel,trace = FALSE)

fitP$linear

fit$linear

### Regularization parameter

error <- with(fitP,Error[Error == min(Error)])
min_err_row <- which(fitP$Error == min(fitP$Error))
temp_lambda <- fitP$lambda[min_err_row]

loc<- which (fitP$lambda[min_err_row] == min(fitP$lambda[min_err_row]))

lambdaP <- temp_lambda[loc]

lambdaP

error[1]/600


#### Training set classification

predP_train <- predict(fitP,newsx = x[train,],lambda = lambdaP,type = "class")

table(predP_train,y[train,],dnn=c("Observed","Predicted"))


##### Test set Classification

predP <- predict(fitP,newsx = x[-train,],lambda=lambdaP,type="class")


table(predP,y[-train,],dnn= c("Observed","Predicted"))
