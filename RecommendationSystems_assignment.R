# Recommendation Systems assignment
# Book dataset
# Problem statement: Recommend a best book based on the ratings.
library(recommenderlab)
library(caTools)

book_rating <- read.csv(file.choose())
View(book_rating)
class(book_rating)
str(book_rating)

# Count of the different books
t <- table(book_rating$Book.Title)
View(t)

# Distribution of the ratings
hist(book_rating$Book.Rating)
# There are most of the books being rated in between 6-10.

book_rating_copy <- book_rating
book_rating <- book_rating[,-1]
View(book_rating)

# Convert to realratingmatrix
book_rating_matrix <- as(book_rating, 'realRatingMatrix')
class(book_rating_matrix)
str(book_rating_matrix)

# Creating a recommendation model
# Two models used for recommendation systems
# Popularity based model
book_rating_recomm_1 <- Recommender(book_rating_matrix, method = "POPULAR")
# Prediction for two users
recommended_books_1 <- predict(book_rating_recomm_1, book_rating_matrix[c(300,1000),], n=3)
as(recommended_books_1, "list")

# Best book to be recommended for the two users: In the Beauty of the Lilies
# Popularity based method will recommend the same books for all users.

# User based model- User Based Collaborative Filtering
book_rating_recomm_2 <- Recommender(book_rating_matrix, method = "UBCF")
# Prediction for one user
recommended_books_2 <- predict(book_rating_recomm_2, book_rating_matrix[c(300,1000)], n=3)
as(recommended_books_2, "list")

# Best book to be recommended for the two users: Jason, Madison &amp

# Conclusion: Both methods differ in performance. The first one will not give an accurate
#             recommendation when compared to the second one.  

