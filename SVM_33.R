# Support Vector Machine
install.packages("kernlab")
library(kernlab)
library(caret)
letters <- read.csv(file.choose())
View(letters)
# For svm all the features must be in numeric
# all the feature values should in same range
# if not we should normalize
# SVM modelwill perform rescalling automatically
# data is randomly arranged
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]
# to train model
# e1071 package from LIBSVMlibrary
# SVMlight algorithm kalr package
# ksvm() function uses gausian RBF kernel
# Building model ( y target variable - lettr)
model1 <- ksvm(letter~.,data = letters_train,kernel = "vanilladot")
model1
help(ksvm)
??ksvm
# Different types of kernels
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot"
# "besseldot", "annovadot", "splinedot", "matrix"

# kernel = rfdot
model_rfdot <- ksvm(letter~.,data = letters_train,kernel = "rbfdot")
pred_rfdot <- predict(model_rfdot,newdata = letters_test)
mean(pred_rfdot==letters_test$letter) # accuracy = 93.075

# Kernel = vanilladot
model_vanilla <- ksvm(letter~.,data = letters_train,kernel = "vanilladot")
pred_vanilla <- predict(model_vanilla,newdata = letters_test)
mean(pred_vanilla==letters_test$letter) # accuracy = 0.83925

# kernel = besseldot
model_bessel <- ksvm(letter~.,data = letters_train,kernel = "besseldot")
pred_bessel <- predict(model_bessel,newdata = letters_test)
mean(pred_bessel==letters_test$letter) # accuracy = 

# kernel = polydot
model_poly <- ksvm(letter~.,data = letters_train,kernel = "polydot")
pred_poly <- predict(model_poly,newdata = letters_test)
mean(pred_poly==letters_test$letter) # accuracy = 83.925 