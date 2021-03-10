# Association Rules assignment
# Problem Statement: Prepare rules for the datasets (book.csv, groceries.csv, my_movies.csv)
#                   1) Try different values of support and confidence. Observe the change
#                      in number of rules for different support,confidence values
#                   2) Change the minimum length in apriori algorithm
#                   3) Visulize the obtained rules using different plots 

library(arules)
library(arulesViz)

#----x----#
# Book dataset
book_data <- read.csv(file.choose())
View(book_data)
summary(book_data)  

book_transaction <- as(as.matrix(book_data), "transactions")
class(book_transaction)
inspect(book_transaction[1:100]) # first 100 transactions

# Generating the rules
# Support = 0.005, Confidence = 0.5, minlen = 2
book_rules <- apriori(as.matrix(book_data), parameter = list(support = 0.005, confidence = 0.5, minlen = 2))
# 3593 rules generated
inspect(book_rules[1:100])                      
plot(book_rules) 

# Support = 0.002, Confidence = 0.2, minlen = 2
book_rules_2 <- apriori(as.matrix(book_data), parameter = list(support = 0.002, confidence = 0.2, minlen = 2))
# 8083 rules generated
inspect(book_rules_2[1:100])                      
plot(book_rules_2) 
r <- sort(book_rules_2, by = "lift")                      
inspect(r[1:100])                     

# Support = 0.01, Confidence = 0.8, minlen = 2
book_rules_3 <- apriori(as.matrix(book_data), parameter = list(support = 0.01, confidence = 0.8, minlen = 2))
# 678 rules generated
inspect(book_rules_3[1:100])                      
plot(book_rules_3) 

# Support = 0.005, Confidence = 0.8, minlen = 3
book_rules_4 <- apriori(as.matrix(book_data), parameter = list(support = 0.005, confidence = 0.8, minlen = 3))
# 1581 rules generated
inspect(book_rules_4[1:100])                      
plot(book_rules_4) 

# Support = 0.005, Confidence = 0.5, minlen = 4
book_rules_5 <- apriori(as.matrix(book_data), parameter = list(support = 0.005, confidence = 0.5, minlen = 4))
# 3290 rules generated
inspect(book_rules_5[1:100])                      
plot(book_rules_5) 

# Support = 0.005, Confidence = 0.5, minlen = 5
book_rules_6 <- apriori(as.matrix(book_data), parameter = list(support = 0.005, confidence = 0.5, minlen = 5))
# 2532 rules generated
inspect(book_rules_6[1:100])                      
plot(book_rules_6)  

# Conclusion: Lowering the confidence level increases the number of rules generated.

#----x----#
# Groceries dataset
groceries <- read.transactions(file.choose(), format = "basket")
inspect(groceries)
class(groceries)

itemFrequencyPlot(groceries)

# Generating rules
# Support = 0.005, confidence = 0.05, minlen = 2
groceries_rules <- apriori(groceries, parameter = list(support = 0.005, confidence = 0.05, minlen = 2))
# 108 rules generated
inspect(groceries_rules)
plot(groceries_rules)

# Support = 0.001, confidence = 0.01, minlen = 2
groceries_rules_2 <- apriori(groceries, parameter = list(support = 0.001, confidence = 0.01, minlen = 2))
# 1567 rules generated
inspect(groceries_rules_2[1:100])
plot(groceries_rules_2)

# Support = 0.001, confidence = 0.005, minlen = 3
groceries_rules_3 <- apriori(groceries, parameter = list(support = 0.001, confidence = 0.005, minlen = 3))
# 487 rules generated
inspect(groceries_rules_3[1:100])
plot(groceries_rules_3)

# Support = 0.001, confidence = 0.005, minlen = 4
groceries_rules_4 <- apriori(groceries, parameter = list(support = 0.001, confidence = 0.005, minlen = 4))
# 64 rules generated
inspect(groceries_rules_4)
plot(groceries_rules_4)
  
 
# Conclusion: By increasing the minlen value, the number of generated rules drops 
#             significantly.

#----x----#
# Movies dataset

movies <- read.csv(file.choose())
View(movies)

binary_data <- movies[,-c(1:5)]
View(binary_data)

# Generating rules
# Support = 0.005, confidence = 0.5, minlen = 2
movie_rules <- apriori(as.matrix(binary_data), parameter = list(support = 0.005, confidence = 0.5, minlen = 2))
# 105 rules generated
inspect(movie_rules)
plot(movie_rules)

# Support = 0.005, confidence = 0.1, minlen = 2
movie_rules_2 <- apriori(as.matrix(binary_data), parameter = list(support = 0.005, confidence = 0.1, minlen = 2))
# 117 rules generated
inspect(movie_rules_2)
plot(movie_rules_2)

# Support = 0.005, confidence = 0.1, minlen = 1
movie_rules_3 <- apriori(as.matrix(binary_data), parameter = list(support = 0.005, confidence = 0.1, minlen = 1))
# 127 rules generated
inspect(movie_rules_3)
plot(movie_rules_3)

# Support = 0.001, confidence = 0.05, minlen = 2
movie_rules_4 <- apriori(as.matrix(binary_data), parameter = list(support = 0.001, confidence = 0.05))
# 127 rules generated
inspect(movie_rules_4)
plot(movie_rules_4)

# Conclusion: By lowering the support and confidence values the maximum number of rules
#             generated for this dataset are 127.  

