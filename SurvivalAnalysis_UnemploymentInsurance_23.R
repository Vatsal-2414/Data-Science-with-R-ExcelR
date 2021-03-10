#Survival Analysis in R
#Unemployment insurance
install.packages('survminer')
install.packages("survival")
library(survminer)
library(survival)

survival_unemployment1 <- read.csv('/Users/vatsalmandalia/survival_unemployment (1).csv')
attach(survival_unemployment1)
str(survival_unemployment1)
View(survival_unemployment1)

#Define variables
time <- spell
event <- event
#X <- cbind(logwage, ui, age)
group <- ui #unemployment insurance can take values 0 or 1
#Descriptive Statistics
summary(time)
summary(event)
#summary(X)
summary(group)
table(group)

#Kaplain-Meier non-paramteric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1) #with respect to those who got insurance
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
ggsurvplot(kmsurvival, data=survival_unemployment1, risk.table = TRUE)

#Kaplain-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time,event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab = "Time", ylab = "Survival Probability")
ggsurvplot(kmsurvival1, data = survival_unemployment1, risk.table = TRUE)
 
########