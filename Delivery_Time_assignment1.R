# Linear Regression assignment 1
# Problem statement: Predict delivery time using sorting time
# Independent variable - Sorting time
# Dependent variable - Delivery time
deltime <- read.csv(file.choose())
View(deltime)
attach(deltime)
library(readr)
library(ggplot2)
# EDA
summary(deltime)
str(deltime)
hist(Delivery.Time)
hist(Sorting.Time)
qqplot(Sorting.Time, Delivery.Time)
cor(Sorting.Time, Delivery.Time) # correlation coefficient = 0.8259973

# Model building
reg_delt <- lm(Delivery.Time~Sorting.Time, data = deltime)
summary(reg_delt)
# R-squared value = 0.6823

# Predictions
pred_delt <- predict(reg_delt)
pred_delt

# Errors
rmse <- sqrt(mean(reg_delt$residuals^2))
rmse

# Visualization of the model
ggplot(data = deltime, aes(x=Sorting.Time, y=Delivery.Time))+geom_point(color='blue')+geom_line(color='red', data = deltime, aes(x=Sorting.Time, y=pred_delt))
# Very poor accuracy of the model


####----Transformation techniques----
# Logarithmic
# x = log(Sorting.Time), y = Delivery.Time
plot(log(Sorting.Time), Delivery.Time)
cor(log(Sorting.Time), Delivery.Time) # correlation coefficient = 0.8339325
hist(log(Sorting.Time))

# Model building
reglog_delt <- lm(Delivery.Time~log(Sorting.Time), data = deltime)
summary(reglog_delt)
# R-squared value = 0.6954

# Predictions
predlog_delt <- predict(reglog_delt)
predlog_delt

# Errors
rmselog <- sqrt(mean(reglog_delt$residuals^2))
rmselog

# Visualization of the model
ggplot(data = deltime, aes(x=log(Sorting.Time), y=Delivery.Time))+geom_point(color='blue')+geom_line(color='red', data = deltime, aes(x=log(Sorting.Time), y=predlog_delt))



# Exponential
# x = Sorting.Time, y = log(Delivery.Time)
plot(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time, log(Delivery.Time)) # correlation coefficient = 0.8431773

# Model building
regexp_delt <- lm(log(Delivery.Time)~Sorting.Time)
summary(regexp_delt)
# R-squared value = 0.7109

# Predictions
predexp_delt <- predict(regexp_delt)
predexp_delt

# Actual values of Delivery.Time
delt <- exp(predexp_delt)
delt

# Errors
error <- Delivery.Time - delt
rmseexp <- sqrt(mean(error^2))
rmseexp

# Visualization
ggplot(data = deltime, aes(x=Sorting.Time, y=log(Delivery.Time)))+geom_point(color='blue')+geom_line(color='red', data = deltime, aes(x=Sorting.Time, y=predexp_delt))



# Degree 2 polynomial
# x = Sorting.Time^2, y = log(Delivery.Time)
plot(Sorting.Time^2, Delivery.Time)
plot(Sorting.Time^2, log(Delivery.Time))
cor(Sorting.Time^2, Delivery.Time) # correlation coefficient = 0.7939063
cor(Sorting.Time^2, log(Delivery.Time)) # correlation coefficient = 0.7882452

# Model building
regpol_delt <- lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time^2), data = deltime)
summary(regpol_delt)
# R-squared value = 0.7649

# Predictions
predpol_delt <- predict(regpol_delt)
predpol_delt

# Actual value of the Delivery.Time
delt_1 <- exp(predpol_delt)
delt_1

# Errors
error_1 <- Delivery.Time - delt_1
rmsepol <- sqrt(mean(error_1^2))
rmsepol

# Visualization of the model
ggplot(data = deltime, aes(x=Sorting.Time+I(Sorting.Time^2), y=log(Delivery.Time)))+geom_point(color='blue')+geom_line(color='red', data = deltime, aes(x=Sorting.Time+I(Sorting.Time^2), y=predpol_delt))



# Degree 3 polynomial
# x = Sorting.Time^3, y = log(Delivery.Time)
plot(Sorting.Time^3, Delivery.Time)
plot(Sorting.Time^3, log(Delivery.Time))
cor(Sorting.Time^3, Delivery.Time) # correlation coefficient = 0.7540763
cor(Sorting.Time^3, log(Delivery.Time)) # correlation coefficient = 0.73172283

# Model building
regpol1_delt <- lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3), data = deltime)
summary(regpol1_delt)
# R-squared value = 0.7819

# Predictions
predpol1_delt <- predict(regpol1_delt)
predpol1_delt

# Actual values
delt_2 <- exp(predpol1_delt)
delt_2

# Errors
error_2 <- Delivery.Time - delt_2
rmsepol1 <- sqrt(mean(error_2^2))
rmsepol1

# Visualization of the model
ggplot(data = deltime, aes(x=Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3), y=log(Delivery.Time)))+geom_point(color='blue')+geom_line(color='red', data = deltime, aes(x=Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3), y=predpol1_delt))


# Final accuracies
accuracy <- c(0.6823, 0.6954, 0.7109, 0.7649, 0.7819)
models <- c("Simple","Logarithmic","Exponential","Degree 2 Polynomial","Degree 3 Polynomial")
print("Prediction models for Delivery.Time:")
df_delt <- data.frame(accuracy,row.names = models)
View(df_delt)

# Conclusion: The degree 3 polynomial model is the best fitted model for this dataset

