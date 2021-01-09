# Libraries
library(readr)
library(leaps)
library(glmnet)
library(dplyr)
library(psych)
library(base)
library(boot)
library(randomForest)
library(caret)
library(mlbench)
library(ROCR)


# Loading the Data
Project_Train <- read_csv("E:/4.3 Semester 3 - Fall '20/ISEN 613 - Engineering Data Analysis/Prof Ceyhun/Group Project/ProjectData.csv")
Project_Test <- read_csv("E:/4.3 Semester 3 - Fall '20/ISEN 613 - Engineering Data Analysis/Prof Ceyhun/Group Project/ProjectTestData.csv")
Data = Project_Train
DataTest = Project_Test
attach(Data)
attach(DataTest)


# Partitioning the data
x.test = model.matrix (`Mean(R)`~ ., DataTest)[,-1]
y.test = DataTest$`Mean(R)`


# Random Forest Regression

# Grid Search for optimal value of 'mtry' for tuning the model
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search ="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(10:15))
rf_gridsearch <- train(`Mean(R)`~., data = Data, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control, ntree = 58)
print(rf_gridsearch) # Optimal mtry value of 14
plot(rf_gridsearch) # Graphical representation


# Grid Search for optimal value of 'ntree' for tuning the model
set.seed(1)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=14)
modellist <- list()
for (ntree in 55:60) {
  set.seed(1)
  fit <- train(`Mean(R)`~., data=Data, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
results <- resamples(modellist) 
summary(results) # comparing the results for optimal value of ntree; ntree = 58 on comparison of test errors


# Estimating the Test Error
set.seed(1)
randomforest = randomForest(`Mean(R)`~ ., data = Data, mtry = 14, ntree = 58, importance = T)
pred.randomforest = predict(randomforest, DataTest)
mean((y.test - pred.randomforest)^2) # Test Error for Random Forest Regression = 0.0063
