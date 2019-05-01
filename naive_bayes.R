
##Collecting data
library(e1071)

set.seed(2016)
num_attr<-2
N<-100
set.seed(2017)
x<-matrix(rnorm(N*num_attr),
          ncol=num_attr)
colnames(x)<-c("x1","x2")
y<-as.numeric((x[,1]^2+x[,2]^2)>2.3)


##Data preparation
class(y)
y<-as.factor(y)

data<-cbind(y,x)
data<-as.data.frame(data)


train<-sample(1:N,70,FALSE)


##Train model using train set
fit<-naiveBayes(x[train,],y[train])


##evaluate model performance
pred_probs<-predict(fit,data[train,-1],type = "raw")
head(pred_probs)


##viewing predicted class labels
pred<-predict(fit,data[train,-1],type = "class")
head(pred)


##confusion matrix

y_train<-y[train]
table(y_train,pred)

pred_test   <-predict(fit,data[-train,-1],type = "class")
head(pred_test)


##confusion matrix
y_test<-y[-train]
table(y_test, pred_test)

