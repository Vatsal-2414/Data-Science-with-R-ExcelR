#Survival Analysis 
#Cancer data
data <- read.csv('/Users/vatsalmandalia/survivalcancerdata.csv')
library(survival)
library(survminer)
View(data)
attach(data)
str(data)
time <- Months_from_diagnosis
event <- Status #has he survived or not
group <- Cell_size #tumor cell sizes
#Descriptive Statistics
summary(time)
summary(event)
#summary(X)
summary(group)
table(group)
kmsurvival <- survfit(Surv(time,event) ~ 1) #w.r.to. cell size 1
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")
ggsurvplot(kmsurvival, data = data, risk.table = TRUE)

kmsurvival1 <- survfit(Surv(time,event) ~ group) #w.r.to. all cell sizes
summary(kmsurvival1)
plot(kmsurvival1, xlab = "Time", ylab = "Survival Probability")
ggsurvplot(kmsurvival1, data = data, risk.table = TRUE)
##########
