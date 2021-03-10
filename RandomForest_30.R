#Random forest
install.packages("randomForest")
library(randomForest)
wbcd <- read.csv(file.choose())
wbcd
View(wbcd)
#First column in dataset is id which is not required so we will be taking out
wbcd <- wbcd[-1]
#table of diagnosis B <- 357, M <- 212
table(wbcd$diagnosis)
#Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))
#table or proportation of entries in the datasets. What % of entry is Benign and % of entry is Malignant
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","texture_mean","perimeter_mean")])
#Create a function to normalize the data
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], norm))
View(wbcd_n)
wbcd_n["diagnosis"] <- wbcd$diagnosis
#Building a random forest model on training data
wbcd_forest <- randomForest(diagnosis~.,data=wbcd_n,importance=TRUE)
plot(wbcd_forest)
legend("topright",colnames(wbcd_forest$err.rate),col = 1:3,cex=0.8,fill = 1:3)
acc_wbcd <- mean(wbcd$diagnosis==predict(wbcd_forest)) #instead of confusion matrix
acc_wbcd
varImpPlot(wbcd_forest)









