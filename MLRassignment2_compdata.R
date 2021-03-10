# Multiple Linear Regression
# Assignment 2
# Problem Statement: Predicting a price for the computer
compdata <- read.csv(file.choose())
compdata_copy <- compdata
View(compdata)
summary(compdata)
str(compdata)
# Converting from boolean to binary factor values
levels(compdata$cd) <- c("0","1")
levels(compdata$multi) <- c("0","1")
levels(compdata$premium) <- c("0","1")
View(compdata)
# loading the libraries
library(readr)
library(ggplot2)
library(psych)
library(car)
library(corpcor)

# Preprocessing the data
hist(compdata$price)
hist(compdata$speed)
hist(compdata$hd)
hist(compdata$ram)
hist(compdata$screen)
boxplot(compdata$price, horizontal = TRUE)
boxplot(compdata$speed)
boxplot(compdata$hd)
library(plyr)
count(compdata$cd)
count(compdata$multi)
count(compdata$premium)

cor(compdata[,-c(1,7,8,9)]) # excluding factor data


#---------------Model building----------------#
attach(compdata)
# Model depending on all variables
model.1 <- lm(price~.-X, data = compdata)
summary(model.1)
# R-squared value = 0.7756

# Model depending on speed, hd, ram
model.1.sp.hd.ram <- lm(price~speed+hd+ram, data = compdata)
summary(model.1.sp.hd.ram)
# R-squared value = 0.4335

# Model depending on speed, hd, ram, cd, multi, premium
model.1.sp.hd.ram.cd.mul.pr <- lm(price~speed+hd+ram+cd+multi+premium, data = compdata)
summary(model.1.sp.hd.ram.cd.mul.pr)
# R-squared value = 0.4816

# Model depending on speed, hd, ram, screen, cd, multi, premium
model.1.sp.hd.ram.screen.cd.mul.pr <- lm(price~speed+hd+ram+screen+cd+multi+premium, data = compdata)
summary(model.1.sp.hd.ram.screen.cd.mul.pr)
# R-squared value = 0.5044

# Model depending on speed, hd, ram, ads, trend
model.1.sp.hd.ram.ads.trend <- lm(price~speed+hd+ram+ads+trend, data = compdata)
summary(model.1.sp.hd.ram.ads.trend)
# R-squared value = 0.6762

# Model depending on screen, cd, multi, premium, ads, trend
model.1.screen.cd.ml.pr.ads.trend <- lm(price~screen+cd+multi+premium+ads+trend, data = compdata)
summary(model.1.screen.cd.ml.pr.ads.trend)
# R-squared value = 0.2776


# Finding collinearity in the data
influence.measures(model.1)
# Vif 
vif(model.1)
# VIF values for the variables < 10
# => There exists no collinearity between the columns

# Added-variable plots
avPlots(model.1, id.n = 2, id.cex = 0.7)

# Influence Index Plot
influenceIndexPlot(model.1, id.n=6)
# Data points 1701 and 1441 show highest residuals

# Influence plots
influencePlot(model.1, id.n=9)
# 1771 and 1441 data points have higher residuals

plot(lm(price~., data = compdata[-c(1441,1701),]))
summary(lm(price~., data = compdata[-c(1441,1701),]))
# R-squared value = 0.78

plot(lm(price~., data = compdata[-c(1441,1701,5961),]))
summary(lm(price~., data = compdata[-c(1441,1701,5961),]))
# R-squared value = 0.7801

# Final model
finalmodel <- lm(price~., data = compdata[-c(1441,1701,5961),])
summary(lm(price~., data = compdata[-c(1441,1701,5961),]))
# R-squared value = 0.7801


#---------------Transformation techniques---------------#
compdata["logprice"] <- log(compdata["price"])
compdata["speed2"] <- compdata["speed"]**2
compdata["speed3"] <- compdata["speed"]**3
compdata["hd2"] <- compdata["hd"]**2
compdata["hd3"] <- compdata["hd"]**3
compdata["ram2"] <- compdata["ram"]**2
compdata["ram3"] <- compdata["ram"]**3
compdata["screen2"] <- compdata["screen"]**2
compdata["screen3"] <- compdata["screen"]**3

class(compdata_copy$cd)
levels(compdata_copy$cd)
co <- as.numeric(as.factor(compdata_copy$cd))
co**2
cdtemp <- as.numeric(compdata["cd"])

compdata["cd2"] <- as.numeric(compdata$cd)**2
compdata["cd3"] <- as.numeric(compdata$cd)**3
compdata["multi2"] <- as.numeric(compdata$multi)**2
compdata["multi3"] <- as.numeric(compdata$multi)**3
compdata["premium2"] <- as.numeric(compdata$premium)**2
compdata["premium3"] <- as.numeric(compdata$premium)**3
compdata["ads2"] <- compdata$ads**2
compdata["ads3"] <- compdata$ads**3
compdata["trend2"] <- compdata$trend**2
compdata["trend3"] <- compdata$trend**3

colnames(compdata)
attach(compdata)
quad_formula <- price~speed+speed2+hd+hd2+ram+ram2+screen+screen2+cd+cd2+multi+multi2+premium+premium2+ads+ads2+trend+trend2
tri_formula <- price~speed+speed2+speed3+hd+hd2+hd3+ram+ram2+ram3+screen+screen2+screen3+cd+cd2+cd3+multi+multi2+multi3+premium+premium2+premium3+ads+ads2+ads3+trend+trend2+trend3
log_formula <- price~log(speed+hd+ram+screen+(as.numeric(cd))+(as.numeric(multi))+(as.numeric(premium))+ads+trend)
exp_formula <- logprice~speed+hd+ram+screen+cd+multi+premium+ads+trend

f <- c(log_formula, exp_formula, quad_formula, tri_formula)
formulae <- lapply(f, as.formula)

####----Logarithmic----####
log_model <- lm(formulae[[1]], data = compdata)
summary(log_model)
# R-squared value = 0.2101

# Predictions
predlog <- predict(log_model)

# RMSE
rmselog <- sqrt(mean(log_model$residuals^2))


####----Exponential----####
exp_model <- lm(formulae[[2]], data = compdata)
summary(exp_model)
# R-squared value = 0.7832

# Predictions
predexp <- predict(exp_model, interval = "confidence")
price_exp <- exp(predexp)

# RMSE
error <- price - price_exp
rmseexp <- sqrt(mean(error^2))


####----Degree 2 Polynomial----####
cor(compdata[,-c(1,7,8,9,14,16,18,20,22,24,26,28,30)])
quadmodel <- lm(formulae[[3]], data = compdata)
summary(quadmodel)
# R-squared value = 0.8035

# Predictions
predpol2 <- predict(quadmodel, interval = "confidence")

# RMSE
rmsepol2 <- sqrt(mean(quadmodel$residuals^2))


####----Degree 3 polynomial----####
trimodel <- lm(formulae[[4]], data = compdata)
summary(trimodel)
# R-squared value = 0.8116

# Predictions
predpol3 <- predict(trimodel, interval = "confidence")

# RMSE
rmsepol3 <- sqrt(mean(trimodel$residuals^2))



# Table containing the R-squared values of the models
models <- c("Simple","Simple (removing collinearity)","Logarithmic model","Exponential model","Degree 2 Polynomial","Degree 3 Polynomial")
Rsquared <- c(0.7756,0.7801,0.2101,0.7832,0.8035,0.8116)
FinalTable <- data.frame(Rsquared, row.names = models)
View(FinalTable)
  




