#install.packages('mclust')
library(mclust)
data(banknote)
head(banknote)

tail(banknote)

table(banknote$Status)


######## Preparation of the data

set.seed(2018)
N = nrow(banknote)
train <- sample(1:N, 150 ,FALSE)

head(train)

###### Train a model using train set
#install.packages('C50')
library(C50)
fitc <- C5.0(Status ~.,data = banknote[train,])
plot(fitc)


fitc_rules <- C5.0(Status ~.,data = banknote[train,],rules = TRUE)

summary(fitc_rules)


######## Evaluate model performance

predc_train <- predict(fitc,newdata = banknote[train,],type = "class")
head(predc_train)

#################confusion matrix
table(banknote$Status[train],predc_train,dnn = c("Observation Class","Predicted Class"))

############### Test set Performance

predc <- predict(fitc,newdata = banknote[-train,],type = "class")

table(banknote$Status[-train],predc,dnn = c("Observation Class","Predicted Class"))



####### Improve model performance
install.packages('tree')
install.packages('tree', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(MASS)
library(tree)
fit <- tree(Status ~.,data = banknote[train,],split = "deviance")

plot(fit); text(fit)

summary(fit)

pred <- predict(fit,newdata = banknote[-train,])
tail(pred,5)

pred.class <- colnames(pred)[max.col(pred,ties.method = c("random"))]

tail(pred.class,5)

###### Confusion matrix

table(banknote$Status[-train],pred.class,dnn = c("Observed Class","Predicted Class"))
