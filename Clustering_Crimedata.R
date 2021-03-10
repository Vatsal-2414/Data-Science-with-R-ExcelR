# Clustering assignment
# Problem statement: Perform clustering for the crime data and identify the number of clusters formed
#                    and draw inferences
library(readxl)
crimedata <- read.csv(file.choose())
View(crimedata)
# Normalizing the data
norm_data <- scale(crimedata[,2:5])
View(norm_data)
# Perfrom hierarchical clustering
d <- dist(norm_data, method = "euclidean")
fit <- hclust(d, method = "complete")
plot(fit, hang = -1)
clusters <- cutree(fit, k=20)
rect.hclust(fit, k=20, border = 'blue')
# 20 clusers introduced for the data
members <- as.matrix(clusters)
final <- data.frame(crimedata, members)
View(final)
# Making the members column as the 1st
final_table <- final[,c(ncol(final), 1:(ncol(final)-1))]
View(final_table)

# Inferences: The number of clusters cannot be equal to the number of rows in the dataset.
#             Having too many clusters is not helpful in this segmentation analysis.
#             A value of k=20 is set for hierarchical clustering. This number seems reliable when
#             looking at the dendogram representation.


########## xxxxx-----------------------x-------------------------xxxxx ###########
# K Means Clustering 
normalised_data <- scale(crimedata[,2:5])

# Model building
fit_1 <- kmeans(normalised_data, 10) # k=10 clusters
fit_1$cluster
membership <- fit_1$cluster
final_1 <- data.frame(crimedata, membership)
View(final_1)
library(data.table)
setcolorder(final_1, neworder = c("membership"))
View(final_1)

# Elbow curve or scree plot, k ~ sqrt(n/2)
# To check which k-value is optimum for this data
# k = sqrt(50/2) = 5
fit_1$withinss # withinss for 10 clusters
library(animation)
fit_1 <- kmeans.ani(normalised_data,10)

wss <- (nrow(normalised_data)-1)*(sum(apply(normalised_data, 2, var)))
for(i in 2:49) wss[i] = sum(kmeans(normalised_data, centers = i)$withinss)
?plot
plot(1:49, wss[1:49], xlab = "Number of clusters", ylab = "Within groups sum of squares", type = 'b')
title(sub = "K means Clustering Scree-Plot")
# For k in the range from 0-10, there is a significant lean bent at a value of 5.
# Therefore, the selected value for k = 5

fit_5 <- kmeans(normalised_data, 5)
membership_5 <- fit_5$cluster
final5 <- data.frame(crimedata, membership_5)
setcolorder(final5, neworder = c("membership_5"))
View(final5)

# Conclusion: k = 5, is chosen as the optimum value for the number of clusters.
# This dataset being small Kmeans clustering is not preferred over Hierarchical clustering.
