# Hierarchical clustering assignment
# Problem statement: Perform clustering (Both hierarchical and K means clustering) for the airlines data
#                    to obtain optimum number of clusters. 
library(readxl)
airlinesdata <- read.csv(file.choose())
View(airlinesdata)
str(airlinesdata)
# Normalizing the data
normalized_data <- scale(airlinesdata[,-1])
View(normalized_data)
# Perform Hierarchical clustering
d <- dist(normalized_data, method = "euclidean")
fit1 <- hclust(d, method = "complete")
plot(fit1, hang = -1)
clusters1 <- cutree(fit1, k=1000)
rect.hclust(fit1, k=1000, border = 'red')
members1 <- as.matrix(clusters1)
final1 <- data.frame(airlinesdata, members1)
View(final1)
final_table1 <- final1[,c(ncol(final1),1:(ncol(final1)-1))]
View(final_table1)


# Inferences: The number of clusters can vary for this data. This dataset is huge with over 4000
#             observations. Therefore Hierarchical clustering does not represent a good option in
#             clustering analysis.   

########## xxxxx-----------------------x-------------------------xxxxx ###########
# K Means Clustering 
norm_data <- scale(airlinesdata[,-1])
View(norm_data)

# Model building
fit <- kmeans(norm_data, 10) # k=10 clusters
fit$cluster
membership <- fit$cluster
final2 <- data.frame(airlinesdata, membership)
View(final2)
library(data.table)
setcolorder(final2, neworder = c("membership"))
View(final2)

# Elbow curve or scree plot, k ~ sqrt(n/2)
# To check which k-value is optimum for this data
fit$withinss # withinss for 10 clusters
library(animation)
fit <- kmeans.ani(norm_data,40)

wss <- (nrow(norm_data)-1)*(sum(apply(norm_data, 2, var)))
for(i in 2:40) wss[i] = sum(kmeans(norm_data, centers = i)$withinss)
?plot
plot(1:20, wss[1:20], xlab = "Number of clusters", ylab = "Within groups sum of squares", type = 'b')
title(sub = "K means Clustering Scree-Plot")
# At different k values, we find lean bents in the plot.
# Therefore, the selected value for k = 10

fit_10 <- kmeans(norm_data, 10)
membership_10 <- fit_10$cluster
final10 <- data.frame(airlinesdata, membership_10)
setcolorder(final10, neworder = c("membership_10"))
View(final10)

# Conclusion: k = 10, is chosen as the optimum value for the number of clusters.
  












