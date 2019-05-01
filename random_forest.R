library(mclust)
data("thyroid",package = "mclust")
head(thyroid)


######## Prepering the data

set.seed(2018)
N=nrow(thyroid)
train<- sample(1:N,150,FALSE)

##########

#install.packages('randomForest')
library (randomForest)
num.trees=800
fit<- randomForest(Diagnosis ~.,
                    data = thyroid [train,],
                    ntree=num.trees,
                    mtry=4)

######### EVALUATE MODEL PERFORMANCE  

print(fit)


########### Test set performance 

fit_test<- randomForest(Diagnosis ~.,
                   data = thyroid [-train,],
                   ntree=num.trees,
                   mtry=4)

print(fit_test)



#### Improve Model performance

fit2<- randomForest(Diagnosis ~.,
                        data = thyroid [-train,],
                        ntree=num.trees,
                        mtry=2,importance=TRUE)
print(fit2)


###### Variable imporatnce 

varImpPlot(fit2)


######## Test dataset 

fit_test2<- randomForest(Diagnosis ~.,
                        data = thyroid [-train,],
                        ntree=num.trees,
                        mtry=2,importance=TRUE)

print(fit_test2)