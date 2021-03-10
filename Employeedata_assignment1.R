# Linear Regression assignments

# Problem Statement: To build a prediction model for Churn_out_rate
# loading the library
library(readr)
# Employee data- To build a churn-out model
empdata <- read.csv(file.choose())
View(empdata)
summary(empdata)
attach(empdata)
# EDA
hist(Salary_hike) # Salary_hike - independent variable
hist(Churn_out_rate) # Churn_out_rate - dependent variable
qqplot(Salary_hike,Churn_out_rate)

# correlation coefficient
cor(Churn_out_rate,Salary_hike)
# -91.1% is the correlation coefficient

# Building a simple linear regression model
reg_churn <- lm(Churn_out_rate~Salary_hike, data = empdata)
summary(reg_churn)
# R-squared value = 0.8312
# Accuracy = 83.12%

# Predicting the Churn_out_rate
pred_churn <- predict(reg_churn)
pred_churn

# Errors
reg_churn$residuals
rmse = sqrt(mean(reg_churn$residuals^2))
rmse

# Confidence interval for a 95% confidence
confint(reg_churn,level = 0.95)
predict(reg_churn, interval = "confidence")

# Plotting the regression model using ggplot
library(ggplot2)
ggplot(data = empdata, aes(x=Salary_hike,y=Churn_out_rate))+geom_point(color='blue')+geom_line(color='red', data = empdata, aes(x=Salary_hike,y=pred_churn))


# To improve accuracy of the model
# carry out transformation techniques
######
# Logarithmic transformation
# x = log(Salary_hike)
# y = Churn_out_rate
plot(log(Salary_hike),Churn_out_rate)
cor(Churn_out_rate,log(Salary_hike))

# Model building
reg_log <- lm(Churn_out_rate~log(Salary_hike), data = empdata)
summary(reg_log)
# R-squared value is 0.8486
# Accuracy of the model = 85%
# Predicting the values of Churn_out_rate
pred_log <- predict(reg_log)
pred_log
# Error calculation
rmse_log <- sqrt(mean(reg_log$residuals^2))
rmse_log
#Confidence interval for 95% confidence
confint(reg_log, level = 0.95)
predict(reg_log, interval = "confidence")
# plotting the model on the data
ggplot(data = empdata, aes(x = log(Salary_hike),y = Churn_out_rate))+geom_point(color='blue')+geom_line(color='red',data=empdata,aes(x=log(Salary_hike),y=pred_log))

#####
# Exponential model
# x=Salary_hike, y=log(Churn_out_rate)
plot(Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
# correlation coefficient = -0.9346
# Model building
reg_exp = lm(log(Churn_out_rate)~Salary_hike, data=empdata)
summary(reg_exp)
# Accuracy of the model = 87.35%
# Prediction using the model
pred_exp <- predict(reg_exp)
pred_exp
# Actual values of Churn_out_rate
churn <- exp(pred_exp)
churn
# errors
error_exp <- Churn_out_rate - churn
rmse_exp <- sqrt(mean(error_exp^2))
rmse_exp
# Plotting the model
ggplot(data = empdata, aes(x=Salary_hike,y=log(Churn_out_rate)))+geom_point(color='blue')+geom_line(color='red',data=empdata,aes(x=Salary_hike,y=pred_exp))

#####
# Polynomial model (2 degree)
# x = Salary_hike^2
# y = log(Churn_out_rate)
plot(Salary_hike^2,Churn_out_rate)
plot(Salary_hike^2,log(Churn_out_rate))
cor(Salary_hike^2,log(Churn_out_rate))
# correlation coefficient = -0.925803
# Model building
reg_pol2 <- lm(log(Churn_out_rate)~Salary_hike+I(Salary_hike^2))
summary(reg_pol2)
# R-squared value = 0.9836
# Accuracy = 98.4%
# Predictions from the model
pred_pol2 <- predict(reg_pol2)
pred_pol2
churn_2 <- exp(pred_pol2)
churn_2
# Error calculations
error_pol2 <- Churn_out_rate - churn_2
rmse_pol2 <- sqrt(mean(error_pol2^2))
rmse_pol2
# Visualizing the model
ggplot(data = empdata, aes(x=Salary_hike+I(Salary_hike^2),y=log(Churn_out_rate)))+geom_point(color='blue')+geom_line(color='red',data = empdata,aes(x=Salary_hike+I(Salary_hike^2),y=pred_pol2))

######
# Polynomial model (3 degree)
plot(Salary_hike^3,Churn_out_rate)
plot(Salary_hike^3,log(Churn_out_rate))
cor(Salary_hike^3,log(Churn_out_rate)) # cor = -0.9164546
cor(Salary_hike^3,Churn_out_rate) # cor = -0.8912506
# Model building
reg_pol3 <- lm(log(Churn_out_rate)~Salary_hike+I(Salary_hike^2)+I(Salary_hike^3))
summary(reg_pol3)
# R-squared value = 0.992
# Accuracy = 99.2%
# Predictions
pred_pol3 <- predict(reg_pol3)
pred_pol3
churn_3 <- exp(pred_pol3)
churn_3
# RMSE calculation
error_pol3 <- Churn_out_rate - churn_3
rmse_pol3 <- sqrt(mean(error_pol3^2))
rmse_pol3
# Model visualization
ggplot(data = empdata, aes(x=Salary_hike+(Salary_hike^2)+I(Salary_hike^3),y=log(Churn_out_rate)))+geom_point(color='blue')+geom_line(color='red',data=empdata,aes(x=Salary_hike+I(Salary_hike^2)+I(Salary_hike^3),y=pred_pol3))

accuracy <- c(83.1, 84.86, 87.35, 98.36, 99.2)
models <- c("Simple","Logarithmic","Exponential","Degree 2 Polynomial","Degree 3 Polynomial")
print("Prediction models for Churn_out_rate:")
df_emp <- data.frame(accuracy,row.names = models)
View(df_emp)
