# Logistic regression
# claimants data
claimants <- read.csv(file.choose())
View(claimants)
#genelarized linear model, multiple independent variables
# one dependent variable, ATTORNEY
fit1 <- glm(ATTORNEY~CLMSEX+CLMINSUR+SEATBELT+CLMAGE+LOSS,data = claimants,family = "binomial")
summary(fit1)
# Linear regression technique can not be employed
prob1 <- predict(fit1,type="response")
#Logistic Regression
logit <- glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS, family = "binomial", data = claimants)
summary(logit)
logit1 <- glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+CLMAGE+LOSS,family = "binomial", data = claimants)
summary(logit1)
# Odds ratio
exp(coef(logit1))
# Confusion matrix table
prob <- predict(logit1, type = c("response"), claimants)
prob
confusion <- table(prob>0.5, claimants$ATTORNEY)
confusion
# Model accuracy
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy
# ROC curve
install.packages("ROCR")
library(ROCR)
rocrpred <- prediction(prob,claimants$ATTORNEY)
rocrperf <- performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#More area under the ROC curve better is the logistic regression model obtained.