# Hypothesis assignment
# Dataset 1: Cutlets dataset
# Problem statement: Is there a difference in the diameter of the cutlets between two units at 5%
#                     significance level.
cutlets_data <- read.csv(file.choose())
View(cutlets_data)
attach(cutlets_data)
# y target variable (Diameter) is continuous
# So we go for second type of tests.

#----Comparing two populations (Unit A and Unit B) with each other----
# -------Normality test-------
library(BSDA)
library(nortest)
library(car)
shapiro.test(Unit.A) # p-value = 0.32>0.05
shapiro.test(Unit.B) # p-value = 0.52>0.05
ad.test(Unit.A)
ad.test(Unit.B)  
# p high <- NULL fly
# Go for NULL hypothesis
# NULL hypothesis - Both sets of values follow normal distribution.

# -------Check if external standards same-------
# Given samples are randomly selected, implies external standards are not same (assumption)

# -------Check if variances are equal-------
var.test(Unit.A, Unit.B) # p-value = 0.3136>0.05
# p high <- NULL fly
# NULL hypothesis - Variances are equal
# ALTERNATIVE hypothesis - Variances are not equal

# -------2 sample T for equal variances-------
t.test(Unit.A, Unit.B, alternative = "two.sided", conf.level = 0.95, var.equal = T)
# p-value = 0.4722>0.05
# p high <- NULL fly
# NULL hypothesis - There is no significant difference in the diameters of both samples
# ALTERNATIVE hypothesis - There is a significant difference in the diameters of both samples
# Summary - No action is needed to be taken by the F&B manager as there is no significant difference
#           in the diameters of both samples.

#-----------x------------#
# Dataset 2: LabTAT.mtw dataset
# Problem statement: Is there a difference in the average TAT of reports among 4 different 
#                    laboratories at 5% significance level.

labtat <- read.csv(file.choose()) # LabTAT dataset
View(labtat)
attach(labtat)
stackeddata <- stack(labtat)
View(stackeddata)
# y variable - values (average TAT of report) - continuous
# x variable - ind (Laboratory no.) - discrete
# Going for second category of tests

#----Comparing two or more (Lab 1,2,3,4) populations with each other----
# -------Normality test-------
shapiro.test(Laboratory.1) # p-value = 0.5508>0.05
shapiro.test(Laboratory.2) # p-value = 0.8637>0.05
shapiro.test(Laboratory.3) # p-value = 0.4205>0.05
shapiro.test(Laboratory.4) # p-value = 0.6619>0.05
# p high <- NULL fly
# NULL hypothesis - All four sets of values have a normal distribution

# -------Variance test-------
leveneTest(stackeddata$values~stackeddata$ind, data = stackeddata) 
# p-value = 0.05161>0.05; p high <- NULL fly, opt for NULL hypothesis
# NULL hypothesis - Variances are equal between the four populations
# ALTERNATIVE hypothesis - Variances are not equal between the four populations

# -------One Way ANOVA test-------
anova_result <- aov(values~ind, data = stackeddata)
?aov
summary(anova_result)
# p-value = 2e-16 < 0.05; p low <- NULL go, opt for Alternative hypothesis
# NULL hypothesis - No difference in average TAT of reports between the 4 labs
# ALTERNATIVE hypothesis - There is a difference in average TAT of reports between the 4 labs
# Summary: The hospital has to take action so that the average TAT of reports remains same between 
#          the 4 labs in its preferred list.  

#-----------x------------#
# Dataset 3: Buyer Ratio dataset
# Problem statement: Check if male-female buyer ratios are same across North, South, West and East
#                    regions.

buyratio <- read.csv(file.choose())
View(buyratio)
attach(buyratio)
buyratio_vals <- buyratio[,-1] # Only numeric values
View(buyratio_vals) # Observed values column removed

#------->2 class variables--------
# Four different class variables (East, West, North, South)
# Hence, chi-square test used
chisq.test(buyratio_vals)
# p-value = 0.6603>0.05 => p high - NULL fly
# NULL hypothesis: Male-Female buyer proportions are equal across all regions
# Alternative hypothesis: Male-Female buyer proportions are unequal across all regions

# Summary: The Male-Female buyer proportions are equal across East, West, North and South regions.
#----------x-----------
# Dataset 4a: Customer Order forms dataset
# Problem Statement: Check if defective % of order forms in 4 centres varies
customer_data <- read.csv(file.choose())
View(customer_data)
customer_temp <- customer_data
attach(customer_temp) # all four columns are of factor type.
# Converting to character type
factor_cols <- sapply(customer_temp, is.factor)
customer_temp[factor_cols] <- lapply(customer_temp[factor_cols], as.character)
sapply(customer_temp, is.character)

# Stacked data
stacked_data <- stack(customer_temp)
View(stacked_data)
attach(stacked_data)
values[values=='Error Free'] <- 0 # Error Free - 0
values[values=='Defective'] <- 1 # Defective - 1
table(values,ind)

#--------Chi Squared test--------
chisq.test(table(values,ind))
# p-value = 0.2771>0.05; p high <- NULL fly
# NULL hypothesis: Defective % of customer order forms is same in the four centres
# Alternative hypothesis: Defective % of customer order forms is different in the four centres

# Summary: The defective % of the customer order forms is the same at 5% significance level
#          in Phillippines, Indonesia, Malta and India.
#-------------x-------------# 
# Dataset 4b: Fantaloons dataset
# Problem statement: To determine whether there is evidence at 5% significance level to support the 
# following hypothesis - % of males versus females walking in the store differ based on day of the
# week.
fantaloons_data <- read.csv(file.choose())
View(fantaloons_data)
fantaloons_copy <- fantaloons_data
attach(fantaloons_copy)
# Converting columns from factor to character type
fac_cols <- sapply(fantaloons_copy, is.factor)
fantaloons_copy[fac_cols] <- lapply(fantaloons_copy[fac_cols], as.character)
sapply(fantaloons_copy, is.character)

# Two populations of Male and Female to compare.
#-------2 Proportion test-------
# Stacking the columns of Weekdays and Weekend
fantaloons_stacked <- stack(fantaloons_copy)
View(fantaloons_stacked)
attach(fantaloons_stacked)
table_fandata <- table(ind,values)
table_fandata
# target variable, x - Weekdays
prop.test(x = c(287,113), n = c(520,280), conf.level = 0.95, correct = FALSE, alternative = "two.sided")
# p-value = 6.261e-05 < 0.05; p low => NULL go    PREFERRED OPTION
prop.test(x = c(287,113), n = c(520,280), conf.level = 0.95, correct = FALSE, alternative = "less")
# p-value = 1 > 0.05; p high => NULL fly

# NULL hypothesis: % of males and females entering in the store do not differ on weekdays and 
#                  weekends
# Alternative hypothesis: % of males and females entering in the store differ on weekdays and 
#                  weekends
# Summary: p-value is less than 0.05 so the NULL hypothesis is rejected. Alternative hypothesis is
#          considered. The % of males and females entering in the store differs based on the day
#          of the week at 5% significance level.
#------------x------------#
 



