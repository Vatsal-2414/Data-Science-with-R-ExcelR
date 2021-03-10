# Linear Regression assignment 1
# Problem statement: Predict weight gained using calories consumed data 
# Independent variable - Calories.Consumed
# Dependent variable - Weight.gained..grams.
calories <- read.csv(file.choose())
View(calories)

library(readr)
library(ggplot2)
attach(calories)

# Exploratory Data Analysis
hist(Calories.Consumed)
hist(Weight.gained..grams.)
plot(Calories.Consumed, Weight.gained..grams.)
summary(calories)
str(calories)
cor(Calories.Consumed, Weight.gained..grams.) # correlation coefficient = 0.946991


# Model building
reg_wtg <- lm(Weight.gained..grams.~Calories.Consumed, data = calories)
summary(reg_wtg)
# R-squared value = 0.8968

# Predictions using the Simple Linear Regression model
pred_wtg <- predict(reg_wtg)
pred_wtg

# Error calculation
rmse <- sqrt(mean(reg_wtg$residuals^2))
rmse

# Visualization of the model on the actual data
ggplot(data = calories, aes(x=Calories.Consumed, y=Weight.gained..grams.))+geom_point(color='blue')+geom_line(color='red', data = calories, aes(x=Calories.Consumed, y=pred_wtg))


########
#----Transformation techniques----
# Logarithmic
# x = log(Calories.Consumed), y = Weight.gained..grams.
plot(log(Calories.Consumed),Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.) # correlation coefficient = 0.8987253

hist(log(Calories.Consumed))

# Model building
reglog_wtg <- lm(Weight.gained..grams.~log(Calories.Consumed), data = calories)
summary(reglog_wtg)
# R-squared value = 0.8077

# Predictions
predlog_wtg <- predict(reglog_wtg)
predlog_wtg

# Errors
rmse_log <- sqrt(mean(reglog_wtg$residuals^2))
rmse_log

# Visualization of the model
ggplot(data = calories, aes(x=log(Calories.Consumed), y=Weight.gained..grams.))+geom_point(color = 'blue')+geom_line(color = 'red', data = calories, aes(x=log(Calories.Consumed), y=predlog_wtg))


# Exponential
# x = Calories.Consumed, y = log(Weight.gained..grams.)
plot(Calories.Consumed, log(Weight.gained..grams.))
plot(Calories.Consumed, Weight.gained..grams.)
cor(Calories.Consumed, log(Weight.gained..grams.)) # correlation coefficient = 0.9368037
hist(log(Weight.gained..grams.))
qqplot(Calories.Consumed, log(Weight.gained..grams.))

# Model building
regexp_wtg <- lm(log(Weight.gained..grams.)~Calories.Consumed, data = calories)
summary(regexp_wtg)
# R-squared value = 0.8776

# Predictions
predexp_wtg <- predict(regexp_wtg)
predexp_wtg

# Actual values of Weight.gained..grams.
wtg <- exp(predexp_wtg)
wtg

# Errors
error <- Weight.gained..grams. - wtg
rmse_exp <- sqrt(mean(error^2))
rmse_exp

# Visualization of the model
ggplot(data = calories, aes(x=Calories.Consumed, y=log(Weight.gained..grams.)))+geom_point(color = 'blue')+geom_line(color = 'red', data = calories, aes(x=Calories.Consumed, y=predexp_wtg))
# There is a outlier closer to a value of 2000 for calories.consumed .


# Degree 2 Polynomial
# x = Calories.Consumed^2, y = Weight.gained..grams.
plot(Calories.Consumed^2, Weight.gained..grams.)
plot(Calories.Consumed^2, log(Weight.gained..grams.))
cor(Calories.Consumed^2, Weight.gained..grams.) # Correlation coefficient = 0.9710636
cor(Calories.Consumed^2, log(Weight.gained..grams.)) # Coreelation coefficient = 0.9267624

qqplot(Calories.Consumed^2, Weight.gained..grams.)

# Model building
regpol_wtg <- lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed^2), data = calories)
summary(regpol_wtg)
# R-squared value = 0.9521

# Predictions
predpol_wtg <- predict(regpol_wtg)
predpol_wtg

# Errors
rmse_pol <- sqrt(mean(regpol_wtg$residuals^2))
rmse_pol

# Visualization of the model
ggplot(data = calories, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=Weight.gained..grams.))+geom_point(color='blue')+geom_line(color='red', data = calories, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=predpol_wtg))



# Degree 3 Polynomial
# x = Calories.Consumed^3, y = Weight.gained..grams.
plot(Calories.Consumed^3, Weight.gained..grams.)
plot(Calories.Consumed^3, log(Weight.gained..grams.))
cor(Calories.Consumed^3, Weight.gained..grams.) # correlation coefficient = 0.971167
cor(Calories.Consumed^3, log(Weight.gained..grams.)) # correlation coefficient = 0.8947014

# Model building
regpol1_wtg <- lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), data = calories)
summary(regpol1_wtg)
# R-squared value = 0.9811
# For log(Weight.gained..grams.)~Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3) model,
# The R-squared value = 0.9417
# The model with highest R-squared value among the two is selected

# Predictions
predpol1_wtg <- predict(regpol1_wtg)
predpol1_wtg

# Errors
rmse_pol1 <- sqrt(mean(regpol1_wtg$residuals^2))
rmse_pol1

# Visualization of the model
ggplot(data = calories, aes(x=Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), y=Weight.gained..grams.))+geom_point(color='blue')+geom_line(color='red', data = calories, aes(x=Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), y=predpol1_wtg))


accuracy <- c(0.8968, 0.8077, 0.8776, 0.9521, 0.9811)
models <- c("Simple","Logarithmic","Exponential","Degree 2 Polynomial","Degree 3 Polynomial")
print("Prediction models for Weight.gained..grams.:")
df_cal <- data.frame(accuracy,row.names = models)
View(df_cal)
# Conclusion: From the above models, the polynomial model of degree 3 is best fitted modeL based on
#             the R-squared value.
