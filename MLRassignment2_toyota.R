# Multiple Linear Regression
# Assignment 2
# Problem statement: Building a prediction model for predicting price for the Toyota Corolla dataset
#                    using only specific columns: "Price","Age_08_04","KM","HP","cc","Doors","Gears",
#                                                 "Quarterly_Tax","Weight"
toyota <- read.csv(file.choose())
View(toyota)
str(toyota)
summary(toyota)

# Loading the libraries
library(readr)
library(ggplot2)
library(psych)
library(car)
library(corpcor)

#-------------Preprocessing the data--------------#
toyota.new <- toyota[,c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(toyota.new)
str(toyota.new)
pairs.panels(toyota.new)
hist(toyota.new$Price)
hist(toyota.new$KM)
hist(toyota.new$HP)
hist(toyota.new$cc)
hist(toyota.new$Age_08_04)
hist(toyota.new$Quarterly_Tax)
hist(toyota.new$Weight)
boxplot(toyota.new$Price)
boxplot(toyota.new$KM)
boxplot(toyota.new$HP)
boxplot(toyota.new$cc)
boxplot(toyota.new$Age_08_04)
boxplot(toyota.new$Quarterly_Tax)
boxplot(toyota.new$Weight)

# Correlation between the variables
cor(toyota.new)
cor2pcor(cor(toyota.new))

#-------------Model building---------------#
# Dependent variable, y = Price
# Independent variables, x = {Age_08_04, KM, HP, cc, Doors, Gears, Quarterly_Tax, Weight}
model.1 <- lm(Price~., data = toyota.new)
summary(model.1)
# R-squared value = 0.8638

# Model depending on Age_08_04, KM, HP
model.1.AgeKMHP <- lm(Price~Age_08_04+KM+HP, data = toyota.new)
summary(model.1.AgeKMHP)
# R-squared value = 0.8103

# Model depending on Age_08_04, KM, HP, Weight
model.1.AgeKMHPWt <- lm(Price~Age_08_04+KM+HP+Weight, data = toyota.new)
summary(model.1.AgeKMHPWt)
# R-squared value = 0.8618

# Model depending on Age_08_04
model.1.Age <- lm(Price~Age_08_04, data = toyota.new)
summary(model.1.Age)
# R-squared value = 0.7684

# Model depending on Age_08_04, KM
model.1.AgeKM <- lm(Price~Age_08_04+KM, data = toyota.new)
summary(model.1.AgeKM)
# R-squared value = 0.79

# Model depending on Age_08_04, HP
model.1.AgeHP <- lm(Price~Age_08_04+HP, data = toyota.new)
summary(model.1.AgeHP)
# R-squared value = 0.8008

# Model depending on Age_08_04, Weight
model.1.AgeWt <- lm(Price~Age_08_04+Weight, data = toyota.new)
summary(model.1.AgeWt)
# R-squared value = 0.8051


# Finding collinearity between the columns
influence.measures(model.1)
# VIF 
vif(model.1)
# VIF values for all the variables < 10.
# => There exists no collinearity between the columns
# Added Variable plot
avPlots(model.1, id.n=2, id.cex = 0.7)
influenceIndexPlot(model.1, id.n=6)
influencePlot(model.1, id.n=9)
# 81st data point has a higher influence than others.

plot(lm(Price~., data = toyota.new[-81,]))
summary(lm(Price~., data = toyota.new[-81,]))
# R-squared value = 0.8694
# Data points 222 and 961 also show higher influence.
# So they are dropped from the model.

plot(lm(Price~., data = toyota.new[-c(81,222),]))
summary(lm(Price~., data = toyota.new[-c(81,222),]))
# R-squared value = 0.8778

plot(lm(Price~., data = toyota.new[-c(81,222,961),]))
summary(lm(Price~., data = toyota.new[-c(81,222,961),]))
# R-squared value = 0.8852


finalmodel.1 <- lm(Price~., data = toyota.new[-c(81,222,961),])
summary(finalmodel.1)
# R-squared value = 0.8852


####-------------Transformation techniques-------------####
toyota.new["logPrice"] <- log(toyota.new["Price"])
toyota.new["Age2"] <- toyota.new["Age_08_04"]**2
toyota.new["Age3"] <- toyota.new["Age_08_04"]**3
toyota.new["KM2"] <- toyota.new["KM"]**2
toyota.new["KM3"] <- toyota.new["KM"]**3
toyota.new["HP2"] <- toyota.new["HP"]**2
toyota.new["HP3"] <- toyota.new["HP"]**3
toyota.new["cc2"] <- toyota.new["cc"]**2
toyota.new["cc3"] <- toyota.new["cc"]**3
toyota.new["Doors2"] <- toyota.new["Doors"]**2
toyota.new["Doors3"] <- toyota.new["Doors"]**3
toyota.new["Gears2"] <- toyota.new["Gears"]**2
toyota.new["Gears3"] <- toyota.new["Gears"]**3
toyota.new["Quarterly_Tax2"] <- toyota.new["Quarterly_Tax"]**2
toyota.new["Quarterly_Tax3"] <- toyota.new["Quarterly_Tax"]**3
toyota.new["Weight2"] <- toyota.new["Weight"]**2
toyota.new["Weight3"] <- toyota.new["Weight"]**3
attach(toyota.new)
View(toyota.new)
quad_formula <- Price~Age_08_04+Age2+KM+KM2+HP+HP2+cc+cc2+Doors+Doors2+Gears+Gears2+Quarterly_Tax+Quarterly_Tax2+Weight+Weight2
tri_formula <- Price~Age_08_04+Age2+Age3+KM+KM2+KM3+HP+HP2+HP3+cc+cc2+cc3+Doors+Doors2+Doors3+Gears+Gears2+Gears3+Quarterly_Tax+Quarterly_Tax2+Quarterly_Tax3+Weight+Weight2+Weight3
log_formula <- Price~log(Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
exp_formula <- log(Price)~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight
f <- c(quad_formula, tri_formula, log_formula, exp_formula)
f
formulae <- lapply(f, as.formula)

####----Logarithmic----####
plot(log(Age_08_04), Price)  
logmodel <- lm(formulae[[3]], data = toyota.new)
summary(logmodel)
# R-squared value = 0.4364

# Predictions
predlog <- predict(logmodel, interval = "confidence")

# Errors
rmselog <- sqrt(mean(logmodel$residuals^2))


####----Exponential----####
plot(Age_08_04, log(Price))
expmodel <- lm(formulae[[4]], data = toyota.new)
summary(expmodel)
# R-squared value = 0.8515

# Predictions
predexp <- predict(expmodel, interval = "confidence")
price.exp <- exp(predexp)
confint(expmodel, level = 0.99)

# Errors
rmseexp <- sqrt(mean((Price - price.exp)^2))


####----Degree 2 Polynomial----####
plot(Age2, Price)
cor(toyota.new[,-c(2,3,4,5,6,7,8,9,10,12,14,16,18,20,22,24,26)])

quadmodel <- lm(formulae[[1]], data = toyota.new)
summary(quadmodel)
# R-squared value = 0.8922

# Predictions
predpol2 <- predict(quadmodel, interval = "confidence")
confint(quadmodel, level = 0.99)

# Errors
rmsepol2 <- sqrt(mean(quadmodel$residuals^2))


####----Degree 3 Polynomial----####
plot(Age3, Price)
cor(toyota.new[,-c(2,3,4,5,6,7,8,9,10,11,13,15,17,19,21,23,25)])

trimodel <- lm(formulae[[2]], data = toyota.new)
summary(trimodel)
# R-squared model = 0.9026

# Predictions
predpol3 <- predict(trimodel, interval = "confidence")
confint(trimodel, level = 0.99)

# Errors
rmsepol3 <- sqrt(mean(trimodel$residuals^2))



# Table containing the R-squared values of the models
models <- c("Simple","Simple (removing collinearity)","Logarithmic model","Exponential model","Degree 2 Polynomial","Degree 3 Polynomial")
Rsquared <- c(0.8638,0.8852,0.4364,0.8515,0.8922,0.9026)
FinalTable <- data.frame(Rsquared, row.names = models)
View(FinalTable)

  

