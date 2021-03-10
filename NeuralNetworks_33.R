# Neural Networks
install.packages("nnet")
install.packages("neuralnet")
library(nnet) # classification
library(neuralnet) # regression
concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
# normal_concrete <- scale(concrete)
# or
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete[,-9],FUN = normalize))
# y target variable used for classification - strenfth 9th column
#summary(conrete_norm$strength)

summary(concrete$strength)
# mean ~ median, strength in normal format
# adding strength col to concrete_norm
concrete_norm <- cbind(concrete_norm,concrete$strength)
colnames(concrete_norm)[9] <- "strength"

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]
# Using multilayered feed forward neural network
# package neuralnet
# Building model
formula_nn <- paste("strength",paste(colnames(concrete[-9]),collapse = "+"),sep="~")
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = concrete_train)
str(concrete_model)
plot(concrete_model)
pred <- concrete_model$net.result
pred_concrete <- predict(concrete_model,newdata = concrete_test)
mean(pred_concrete==concrete_test$strength) # accuracy = 0, model generated, but output is not the required one.
# so go for model evaluation

#SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
str(model_results)
predicted_strength <- model_results$net.result
# predicted_strength
model_results$neurons
cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)
# now using 5 hidden layers
model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5) 
#ERROR!!!!!!!!!!!!!!!!!!!!!!! 
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)
plot(pred_strn_5,concrete_test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
