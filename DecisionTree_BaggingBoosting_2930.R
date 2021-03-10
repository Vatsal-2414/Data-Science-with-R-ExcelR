#Decision tree
#iris dataset
data("iris")
library(caret)
install.packages("c5O")
library(C50)
# Data partition for model building and testing
inTraininglocal <- createDataPartition(iris$Species,p=0.75,list=F) 
training <- iris[inTraininglocal,] # 75% data
test <- iris[-inTraininglocal,] # 25% data
# model building
model <- C5.0(training$Species~.,data=training) # y-target is species
# generating the model summary
summary(model)
pred <- predict.C5.0(model,test[,-5]) #predict using the test data (5th column of species not considered)
a <- table(test$Species,pred)
a
#confusion matrix, predicted correctly or not
sum(diag(a)/sum(a))
#with 86% accuracy predicting right
plot(model)


#####Bagging#####
acc <- c()
acc
for(i in 1:100)
{
  print(i)
  inTraininglocal <- createDataPartition(iris$Species,p=0.85,list=F)
  training1 <- iris[inTraininglocal,]
  testing <- iris[-inTraininglocal,]
  
  fittree <- C5.0(training1$Species~.,data=training1)
  pred <- predict.C5.0(fittree,testing[,-5])
  a <- table(testing$Species,pred)
  
  acc <- c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
 