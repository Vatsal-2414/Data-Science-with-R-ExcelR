# Session 10
# Hierarchical clustering
library(readxl) #package to load data from xl file
input <- read_excel("/Users/vatsalmandalia/Universities_Clustering.csv")
View(input)
mydata <- input[1:25,c(1,3:8)] #not state
View(mydata)
normalized_data <- scale(mydata[,2:7]) #Excluding university name from normalization
d <- dist(normalized_data, method="euclidean") #Distance matrix
fit <- hclust(d,method="complete")
?hclust
plot(fit) #Display dendogram
plot(fit,hang=-1)
groups <- cutree(fit,k=5)
?cutree
rect.hclust(fit,k=5,border="red")
?rect.hclust
membership <- as.matrix(groups)
final <- data.frame(mydata,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))] #to shift membership column from last to close to Universities name column
View(final1)  
