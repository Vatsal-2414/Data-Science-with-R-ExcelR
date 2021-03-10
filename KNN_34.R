# K Nearest Neighbours classification
# Read the dataset
wbcd <- read.csv(file.choose())
class(wbcd)
# first column is id, which is not required so we will be taking out
wbcd <- wbcd[-1]
View(wbcd)
str(wbcd)
# table of diagnosis, B <- 357 M <- 212
table(wbcd$diagnosis)
# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M.
# 
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))
# table or proportion of entries in the datasets, what % of entry is Benign and Malignant
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","texture_mean","perimeter_mean")])
# Create a function to normalize the data
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
# Normalizing the data
wbcd_n <- as.data.frame(lapply(wbcd[2:31],norm))
View(wbcd_n)

# data segregation
wbcd_training <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
# Get labels for training and test datasets
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
wbcd_train_labels

# Build a KNN model on training dataset
library(class)
library(caret)
# Building the KNN model on training dataset and also need labels which we are including in cl.
# we have to test on the test dataset
wbcd_pred <- knn(train = wbcd_training, test = wbcd_test, cl = wbcd_train_labels, k=21)
plot(wbcd_pred)
class(wbcd_training)
class(wbcd_test)
# Now evaluating the model performance
install.packages("gmodels")
library(gmodels)
# Create cross table of predicted and actual
CrossTable(x = wbcd_test_labels, y = wbcd_pred)
# Model correctly classified 61% of entries as Benign and 37% as Malignant. 
# But it misclassified 2% as Benign



