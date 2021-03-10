#installing and loading the required libraries
install.packages("recommenderlab")
install.packages("caTools")
library(recommenderlab)
library(caTools)
#movie rating data
movie_rating_data <- read.csv("/Users/vatsalmandalia/data1.csv")
View(movie_rating_data)
class(movie_rating_data)
#metadata about the new variable
str(movie_rating_data)
table(movie_rating_data$movie)
#rating distribution
hist(movie_rating_data$rating)

#the datatype should be realRatingMatrix in order to build a recommendation engine
movie_rate_data_matrix <- as(movie_rating_data, 'realRatingMatrix')
#Popularity based
movie_recomm_model1 <- Recommender(movie_rate_data_matrix, method="POPULAR")
#Predictions for two users
recommended_items1 <- predict(movie_recomm_model1, movie_rate_data_matrix[413:414],n=5) #n=5, no. of recommendations
as(recommended_items1,"list")
## Popularity model recommends the same movies for all users, we need to improve our model
# using # # Collborative Filtering
#User based Collaborative Filtering
movie_recomm_model2 <- Recommender(movie_rate_data_matrix, method="UBCF")
#Predictions for two users
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix[413:414], n=5)
as(recommended_items2,"list") 
