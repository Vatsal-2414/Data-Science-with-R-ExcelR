# Principal Component Analysis (PCA) assignment
# Problem statement: Perform Principal component analysis and perform clustering using 
# first 3 principal component scores (both hierarchial and k mean clustering(scree plot
# or elbow curve) and obtain optimum number of clusters and check whether we have 
# obtained same number of clusters with the original data.

# Wine dataset
wine_data <- read.csv(file.choose())
View(wine_data)
attach(wine_data)
cor(wine_data)
str(wine_data)

# Creating Principal Component Objects
pca_obj <- princomp(wine_data, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca_obj)
str(pca_obj)
plot(pca_obj)
# From comp.1 to comp.3 the cumulative proportion of variance is almost 67.7%. The rest
# of the components contribute a small fraction of the variance.
# For carrying out the data analysis the first 3 components are chosen with excluding
# the rest. 
# Majority of the information lies in the first three components.
biplot(pca_obj)
pca_obj$scores[,1:3]
# Binding the three components to the dataset
wine_data_pca <- cbind(wine_data, pca_obj$scores[,1:3])
View(wine_data_pca)

#----x----#
# Performing Clustering analysis on the data (the three components)
# Hierarchical Clustering
hclus_data <- wine_data_pca[,15:17]
View(hclus_data)

# Normalizing the data
norm_hclus <- scale(hclus_data)
# Calculating euclidean distance
dist_hclus <- dist(norm_hclus, method = "euclidean")
# Clustering model
fit1 <- hclust(dist_hclus, method = "complete")
plot(fit1)

# Number of clusters = 5
groups1 <- cutree(fit1, 5)
members1 <- as.matrix(groups1)
View(members1)
final1 <- cbind(members1, wine_data_pca)
View(final1)
mean_final1 <- aggregate(final1[,-c(16:18)], FUN = mean, by = list(members1))
View(mean_final1)

# Number of clusters = 15
groups2 <- cutree(fit1,15)  
members2 <- as.matrix(groups2)
View(members2)
final2 <- cbind(members2, wine_data_pca)
View(final2)
mean_final2 <- aggregate(final2[,-c(16:18)], FUN = mean, by = list(members2))
View(mean_final2)

# Number of clusters = 20
groups3 <- cutree(fit1,20)  
members3 <- as.matrix(groups3)
View(members3)
final3 <- cbind(members3, wine_data_pca)
View(final3)
mean_final3 <- aggregate(final3[,-c(16:18)], FUN = mean, by = list(members3))
View(mean_final3)

#----x----#
# K-means clustering
# Considering the first seven principal components
kmeans_data <- wine_data_pca[,15:17]
View(kmeans_data)

# Normalising the data
norm_kmeans <- scale(kmeans_data)
View(norm_kmeans)

# Elbow curve/Scree plot
# To determine the 'k' value for the number of clusters (k ~ sqrt(n/2))
k_empirical <- sqrt(nrow(wine_data)/2) # Empirical k ~ 9

k_model <- kmeans(norm_kmeans,5)
wss <- (nrow(norm_kmeans)-1)*sum(apply(norm_kmeans,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(norm_kmeans, centers = i)$withinss) 
plot(1:20, wss[1:20], xlab = "No. of clusters, k", ylab = "within sum of squares",type = 'b')
title("Elbow curve/Scree plot")
# The plot contains a couple of breaking points at k=5 and k=8.
# This indicates the estimated value of the number of clusters is either 5 or 8.
      
# Model building
# No. of clusters = 5      
fit_km1 <- kmeans(norm_kmeans,5)       
final1_km <- data.frame(wine_data, fit_km1$cluster)
View(final1_km)
library(data.table)
setcolorder(final1_km, neworder = c("fit_km1.cluster"))
mean_final1_km <- aggregate(wine_data, by = list(fit_km1$cluster), FUN = mean)
View(mean_final1_km)
fit_km1$size

# No. of clusters = 8
fit_km2 <- kmeans(norm_kmeans,8)
final2_km <- data.frame(wine_data, fit_km2$cluster)
View(final2_km)
setcolorder(final2_km, neworder = c("fit_km2.cluster"))
mean_final2_km <- aggregate(wine_data, by = list(fit_km2$cluster), FUN = mean)
View(mean_final2_km)
fit_km2$size

#----x----#
# Conclusion: The optimum number of clusters is 5. By comparing the K-means and 
#             Hierarchical clustering methods, the optimum value for k = 5. This value
#             is determined using only the first three principal components.
     

