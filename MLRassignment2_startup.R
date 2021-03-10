# Multiple Linear Regression
# Assignment 2
# Problem statement: Prepare a prediction model for profit of 50_startups data
library(ggplot2)
library(readr)
library(psych)
library(corpcor)

# Loading the data and doing EDA
startup_data <- read.csv(file.choose())
View(startup_data)
attach(startup_data)
summary(startup_data)
str(startup_data)

pairs.panels(startup_data)
class(startup_data)
# Neglecting the 'State' column
startup <- startup_data[,-4]
View(startup)

cor(startup)
cor2pcor(cor(startup))

# Model generation
model1 <- lm(Profit~., data = startup)
summary(model1)
# R-squared value = 0.9507

# Model based on R&D spend
model1_RD <- lm(Profit~R.D.Spend, data = startup)
summary(model1_RD)
# R-squared value = 0.9465

# Model based on Administration
model1_adm <- lm(Profit~Administration, data=startup)
summary(model1_adm)
# R-squared value = 0.04029

# Model based on Marketing.Spend
model1_mark <- lm(Profit~Marketing.Spend, data = startup)
summary(model1_mark)
# R-squared value = 0.5592

# Model based on R&D spend and Marketing
model1_RDMark <- lm(Profit~R.D.Spend+Marketing.Spend, data = startup)
summary(model1_RDMark)
# R-squared value = 0.9505


# To find the collinearity between the independent variables
# Influence Plots
influence.measures(model1) 
# Variance Inflation Factor
library(car)
vif(model1)
# Vif values for the 3 independent variables < 10
# => There exists no collinearity between them
avPlots(model1, id.n=2, id.cex = 0.7)
influenceIndexPlot(model1, id.n=6)
# 49th and 50th data points show highest residuals
influencePlot(model1, id.n=9)

model1 <- lm(Profit~., data = startup[-50,]) # neglecting 50th data point
summary(model1)
# R-squared value = 0.9613
model1 <- lm(Profit~., data = startup[-c(49,50),])
summary(model1)
# R-squared value = 0.9627
influencePlot(model1, id.n=9)
influenceIndexPlot(model1, id.n=6)

# Final model
plot(lm(Profit~., data = startup[-50,])) # without 50th data point
summary(lm(Profit~., data = startup[-50,]))
plot(lm(Profit~., data = startup[-c(49,50),])) # without 49 and 50 data points
summary(lm(Profit~., data = startup[-c(49,50),]))

finalmodel <- lm(Profit~., data = startup[-c(49,50),])
summary(finalmodel)
# Simple model
# R-squared value = 0.9627

plot(finalmodel)
hist(finalmodel$residuals)



#####----------Transformation techniques--------#####
startup["logProfit"] <- log(startup[,"Profit"])
startup["R.D.Spend2"] <- startup["R.D.Spend"]**2
startup["R.D.Spend3"] <- startup["R.D.Spend"]**3  
startup["Marketing.Spend2"] <- startup["Marketing.Spend"]**2
startup["Marketing.Spend3"] <- startup["Marketing.Spend"]**3
startup["Administration2"] <- startup["Administration"]**2
startup["Administration3"] <- startup["Administration"]**3
attach(startup)

quad_formula <- Profit~R.D.Spend+R.D.Spend2+Marketing.Spend+Marketing.Spend2+Administration+Administration2
tri_formula <- Profit~R.D.Spend+R.D.Spend2+R.D.Spend3+Marketing.Spend+Marketing.Spend2+Marketing.Spend3+Administration+Administration2+Administration3
f <- c(quad_formula,tri_formula)
formulae <- lapply(f, as.formula)

####-------Logarithmic model-------####
cor(startup[,-c(5,6,7,8,9,10,11)])
logmodel <- lm(Profit~log(R.D.Spend + Marketing.Spend + Administration), data = startup[,-c(5,6,7,8,9,10,11)])
summary(logmodel)
# R-squared value = 0.669

# Predictions and actual values
predlog <- predict(logmodel, interval = "confidence")
confint(logmodel, level = 0.99)

# RMSE
rmse.log <- sqrt(mean(logmodel$residuals^2))

####-------Exponential model-------####
expmodel <- lm(logProfit~., data = startup[,-c(5:11)])
summary(expmodel)
# R-squared value = 0.9033

# Predictions and actual values
predexp <- predict(expmodel)
profit.exp <- exp(predexp)
confint(expmodel, level = 0.99)
predict(expmodel, interval = "confidence")

# RMSE
error.exp <- Profit - profit.exp
rmse.exp <- sqrt(mean(error.exp^2))

####------Degree 2 polynomial model------####
cor(startup[,-c(1,2,3,7,9,11)])
quadmodel <- lm(formulae[[1]], data = startup)
summary(quadmodel)
# R-squared value = 0.9516, for y = Profit
# R-squared value = 0.8303, for y = log(Profit)

# Predictions and actual values
predpol2 <- predict(quadmodel, interval = "confidence")
confint(quadmodel, level = 0.99)

# RMSE
rmse.2 <- sqrt(mean(quadmodel$residuals^2))

####-------Degree 3 polynomial model-------####
cor(startup[,-c(1,2,3,6,8,10)])
plot(R.D.Spend3, logProfit)
trimodel <- lm(formulae[[2]], data = startup)
summary(trimodel)
# R-squared value = 0.9634, for y = Profit
# R-squared value = 0.881, for y = log(Profit)

# Predictions and actual values
predpol3 <- predict(trimodel, interval = "confidence")
confint(trimodel, level = 0.99)

# RMSE
rmse.3 <- sqrt(mean(trimodel$residuals^2))



# Table containing the R-squared values of the models
models <- c("Simple","Logarithmic model","Exponential model","Degree 2 Polynomial","Degree 3 Polynomial")
Rsquared <- c(96.27,66.9,90.33,95.16,96.34)
FinalTable <- data.frame(Rsquared, row.names = models)
View(FinalTable)
 

