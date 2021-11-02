# Name: Shobhakhar Adhikari
# R Human Resource Project
#============================================================================================================
# Set the Directory and load the dataset into R, verify that the data is loaded correctly
setwd(choose.dir())
getwd()

data <- read.csv("HR_data.csv")
View(data)
#=============================================================================================================
# For finding the insights out of our data several techniques can be used
# Find the correlation values of the attributes of our data
# Visualize the characteristics of the whole data and only the people who left, use plots
# and histograms
# Evaluate the values of each attributes for both left and non-left employees
# Analyse the department wise turnouts and find out the percentage of employees
# leaving from each department

str(data)
data$department <- as.factor(data$department)
data$salary <- as.factor(data$salary)
data$left <- as.factor(data$left)
table(data$department)
library(ggplot2)
library(dplyr)
data_left = filter(data, left ==1)
View(data_left)
table (data_left$salary)
table(data_left$department)
table(data_left$time_spend_company)
table(data_left$promotion_last_5years)
data$promotion_last_5years <- as.factor(data$promotion_last_5years)

ggplot(data, aes(department, fill = left))+geom_bar(position = "stack")
ggplot(data, aes(department, fill = left))+geom_bar(position = "fill")
ggplot(data, aes(left, fill = department))+geom_bar(position = "fill")
ggplot(data_left,aes(left,fill=department))+geom_bar(position = 'dodge')
ggplot(data_left,aes(left,fill=salary))+geom_bar(position = 'dodge')

ggplot(data_left,aes(left,fill= promotion_last_5years))+geom_bar(position = 'dodge')
# more than 99% who left did not have promotion last 5 years.


# more than 2500 employees with low salary left
# less than 100 employees with high salary left
# more than 1000 employees of sales department left(largest %)
# less than 100 employees of Management department left(smallest %)

ggplot(data, aes(left, fill = department))+geom_bar(position = "dodge")
ggplot(data, aes(left, fill = salary))+geom_bar(position = "dodge")
ggplot(data,aes())
table (data$left)
# 3571 employess left the job
#==================================================================================================================
# Build a classification model to forecast what are the attributes of people who leave the
# company
# Build models using Decision Tree, Random Forest, Naïve Bayes and SVM
# techniques and find out the most accurate one

#Build a training and testing set
set.seed(4)
id <- sample(1:nrow(data), 0.7*nrow(data))  # row indices for training data
trainset <- data[id, ]  # model training data
testset  <- data[-id, ]   # test data

#Build logistic regression model
model<-glm(trainset$left~. ,data = trainset, family='binomial')
summary(model)

predvalues<-predict(model,newdata = testset,type = "response")
predvalues

table(Actualvalues = testset$left, Predictedvalues = predvalues>0.4)
# accuracy = 79.31%
#========================================================================================================

#Build Decision Tree
library(rpart)

#Build decision tree model)
data$left <- factor(data$left, levels=c(0,1), labels=c("No", "Yes"))
View(data)


#Build a training and testing set
set.seed(4)
id <- sample(1:nrow(data), 0.7*nrow(data))  # row indices for training data
trainset <- data[id, ]  # model training data
testset  <- data[-id, ]   # test data

model1<-rpart(trainset$left~. ,data = trainset)
summary(model1)
plot(model1, margin=0.1)
text(model1, use.n = TRUE,pretty = TRUE, cex=0.8)
predvalues1<-predict(model1,newdata = testset,type = "class")
predvalues1

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(table(predvalues1, testset$left))
class(predvalues1)
class(testset$left)
table(predvalues1)
# Accuracy 97.22%
#====================================================================================================================
# Random Forest Model
install.packages("randomForest")
library(randomForest)

#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(data),prob = c(0.7,0.3),replace = T)
trainset<-data[id==1,]
testset<-data[id==2,]
View(data)
class(trainset)

#Build random forest model
model2<-randomForest(trainset$left~. ,data = trainset, ntree=100)
model2



predvalues<-predict(model2,newdata = testset,type = "class")
predvalues

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)
confusionMatrix(table(predvalues, testset$left))
# 99% accuracy

varImp(model)
varImpPlot(model2,sort=TRUE, type=2)
importance(model2)
varUsed(model2)
# satisfaction_level var has maximum Mean Decrease Gini (important variable for employees leaving the company)
#=========================================================================================================================

#Build naive bayes model
model3<-naiveBayes(trainset$left~. ,data = trainset)
model3

predvalues<-predict(model3,newdata = testset,type = "class")
predvalues

confusionMatrix(table(predvalues, testset$left))
# Accuracy : 84.57%  
#===========================================================================================================

#Build support vector machines


#kernel can be linear/polynomial/radial or sigmoid
#cost called as c value - determine the width of the margin, larger the c value, smaller the width
#scale for normalization to avoid bias

model4<-svm(trainset$left~. ,data = trainset, kernel = "polynomial", cost = 0.1)
summary(model4)


predvalues<-predict(model4,newdata = testset,type = "class")
predvalues

confusionMatrix(table(predvalues, testset$left))
# Accuracy : 92.88%
#===========================================================================================================
# Best Model : Random Forest with 99% Accuracy. (Based on Accuracy!!)


