# Linear regression model
# Salary_hike data
salarydata <- read.csv(file.choose())
View(salarydata)
summary(salarydata)
attach(salarydata)
library(readr)
library(ggplot2)

# Exploratory Data Analysis
hist(YearsExperience)
hist(Salary)
cor(YearsExperience,Salary) # Correlation coefficient = 0.9782416
qqplot(YearsExperience,Salary)
# independent variable - Years of experience
# dependent variable - Salary

# Simple Linear Regression Model
reg_sal <- lm(Salary~YearsExperience, data = salarydata)
summary(reg_sal)
# R-squared value = 0.957

# Predicting the salary_hike using the model
pred_sal <- predict(reg_sal)
pred_sal
# Errors
rmse_sal <- sqrt(mean(reg_sal$residuals^2))
rmse_sal

# Confidence interval for 95% confidence level
confint(reg_sal,level = 0.95)
predict(reg_sal, interval = "confidence")
predict(reg_sal, interval = "prediction")

# Visualizing the model
ggplot(data = salarydata, aes(x=YearsExperience,y=Salary))+geom_point(color='blue')+geom_line(color='red', data = salarydata, aes(x=YearsExperience,y=pred_sal))


####
# Transformation techniques to improve the R-squared value
# Logarithmic transformation
# x = log(YearsExperience), y = Salary
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary) # correlation coefficient = 0.9240611
hist(log(YearsExperience))
qqplot(log(YearsExperience),Salary)

# Simple Linear Regression model
reglog_sal <- lm(Salary~log(YearsExperience), data = salarydata)
summary(reglog_sal)
# R-squared value = 0.8539

# Predicting the salary
predlog_sal <- predict(reglog_sal)
predlog_sal

# Errors
rmse_log <- sqrt(mean(reglog_sal$residuals^2))
rmse_log

# Confidence interval for 95% confidence
confint(reglog_sal,level = 0.95)

# Visualization of the model 
ggplot(data = salarydata, aes(x=log(YearsExperience),y=Salary))+geom_point(color='blue')+geom_line(color='red',data = salarydata, aes(x=log(YearsExperience),y=predlog_sal))


##
# Exponential transformation
# x = YearsExperience, y = log(Salary)
plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary)) # correlation coefficient = 0.9653844
qqplot(YearsExperience, log(Salary))
hist(log(Salary))

# SLR model
regexp_sal <- lm(log(Salary)~YearsExperience, data = salarydata)
summary(regexp_sal)
# R-squared value = 0.932

# Predictions
predexp_sal <- predict(regexp_sal)
predexp_sal

# Actual values of the Salary
salary_1 <- exp(predexp_sal)
salary_1

# Errors
error_1 <- Salary - salary_1
rmse_exp <- sqrt(mean(error_1^2))
rmse_exp

# Visualizing the model on the data
ggplot(data = salarydata, aes(x=YearsExperience, y=log(Salary)))+geom_point(color = 'blue')+geom_line(color = 'red', data = salarydata, aes(x=YearsExperience, y=predexp_sal))



###
# Polynomial model of degree 2
plot(YearsExperience^2, Salary)
plot(YearsExperience^2, log(Salary))
cor(YearsExperience^2, Salary) # correlation coefficient = 0.9567235
cor(YearsExperience^2, log(Salary)) # correlation coefficient = 0.9157747

# Model building
regpol1 <- lm(log(Salary)~YearsExperience+I(YearsExperience^2), data = salarydata)
summary(regpol1)
# R-squared value = 0.9486

# Predicting the salary for the employees
predpol1 <- predict(regpol1)
predpol1

# Actual value of the salary
salary_2 <- exp(predpol1)
salary_2

# Error calculation
error_2 <- Salary - salary_2
rmse_pol <- sqrt(mean(error_2^2))
rmse_pol

# Visualization of the model
ggplot(data = salarydata, aes(x=YearsExperience+I(YearsExperience^2), y=log(Salary)))+geom_point(color='blue')+geom_line(color='red',data = salarydata, aes(x=YearsExperience+I(YearsExperience^2), y=predpol1))



###
# Polynomial model of degree 3
# x = YearsExperience^3, y = log(Salary)
plot(YearsExperience^3, Salary)
plot(YearsExperience^3, log(Salary))
cor(YearsExperience^3, Salary) # correlation coefficient = 0.9133658
cor(YearsExperience^3, log(Salary)) # correlation coefficient = 0.8565161

hist(YearsExperience^3)

# Model building for salary prediction
regpol2 <- lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3), data = salarydata)
summary(regpol2)
# R-squared value = 0.9636
# The model of log(Salary) is not considered due to a low R-squared value of it compared to regpol2.
 
# Predicting the salary
predpol2 <- predict(regpol2)
predpol2

# Errors
rmse_pol2 <- sqrt(mean(regpol2$residuals^2))
rmse_pol2

# Visualization of the model
ggplot(data = salarydata, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3),y=Salary))+geom_point(color='blue')+geom_line(color='red', data=salarydata, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3),y=predpol2))

# Final accuracy of the models
accuracy <- c(0.957, 0.8539, 0.932, 0.9486, 0.9636)
model <- c("Simple", "Logarithmic", "Exponential", "Degree 2 Polynomial", "Degree 3 Polynomial")
df_sal <- data.frame(accuracy, row.names = model)
View(df_sal)
 





