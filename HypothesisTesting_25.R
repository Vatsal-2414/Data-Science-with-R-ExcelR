#Hypothesis testing
####### 1 sample z test ########
fabric <- read.csv('/Users/vatsalmandalia/Fabric_data.mtw.csv')
View(fabric)
###### Normality test ######
install.packages("BSDA")
library(BSDA)
install.packages("nortest")
library(nortest)
ad.test(fabric$Fabric_length)  #Anderson-Darling test
# p high, null fly
######### 1 sample z test (since sigma known) #######
z.test(fabric$Fabric_length, alternative = "greater", mu = 0, sigma.x = 4, sigma.y = NULL, conf.level = 0.95)
# p value < 0.05
# should go for alternative hypothesis, another strategy

############ 1 sample t test (sigma not known) ##########
bolt_d <- read.csv('/Users/vatsalmandalia/Bolt_Diameter.csv')
######Normality test##########
library(nortest)
ad.test(bolt_d$Diameter) # p high, null high
######### 1 sample t-test ########
t.test(bolt_d$Diameter, mu = 0, alternative = "greater") # p low, NULL go
# go for alternative hypothesis


####### 2 sample T test ########
Promotion <- read.csv('/Users/vatsalmandalia/Promotion.csv')
View(Promotion)
attach(Promotion)
colnames(Promotion) <- c("Credit","Promotion.Type","InterestRateWaiver","StandardPromotion")
# changing column names
View(Promotion)
attach(Promotion)
############# Normality Test############
shapiro.test(InterestRateWaiver)
# p value = 0.2246 >0.05, so p high, NULL fly => It follows normal distribution
shapiro.test(StandardPromotion)
# p value = 0.1916>0.05, p high, NULL fly => It follows normal distribution
###############variance test############
var.test(InterestRateWaiver,StandardPromotion)
# p value =0.653>0.05, so p high, NULL fly => equal variances
########## 2 sample T test########
t.test(InterestRateWaiver,StandardPromotion,alternative = "two.sided",conf.level = 0.95,correct = TRUE) # two sided
?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "greater", var.equal = T)
# pvalues from both ways < 0.05, so we will not expect full purchases from Interest rate waiver.


