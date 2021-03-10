cars_data <- read.csv('/Users/vatsalmandalia/Cars.csv')
View(cars_data)

mpg <- cars_data$MPG
mpg

#Mean, stddev
mu = mean(mpg)
variance = var(mpg)
variance
std = sqrt(variance)
std
attach(cars_data)
hist(mpg)
qqnorm(mpg)
qqline(mpg)

library(e1071)
skewness(mpg)
kurtosis(mpg)

wcat <- read.csv('/Users/vatsalmandalia/wc-at.csv')
View(wcat)
attach(wcat)
hist(AT)
hist(Waist)
skewness(AT)
kurtosis(AT)
skewness(Waist)
kurtosis(Waist)

qqnorm(AT)
qqnorm(Waist)
qqline(Waist)

qnorm(0.80)
qnorm(0.95)
qnorm(0.97)
