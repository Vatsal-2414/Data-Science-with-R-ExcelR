# Wordcloud generation
# Session 16
install.packages("tm") #for text mining
install.packages("SnowballC") #for text stemming
install.packages("wordcloud") #for wordcloud generation
install.packages("RColorBrewer")
#Load 
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
text <- readLines(file.choose()) #modi.txt
View(text)
docs <- Corpus(VectorSource(text)) # each row = document, group of documents = corpus
inspect(docs)
#function to remove unnecessary words
toSpace <- function(x, pattern) gsub(pattern, " ", x) # gsub- matches the pattern, replaces it with a space
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#Convert the text to lower case
docs <- tm_map(docs, tolower)
#Remove numbers
docs <- tm_map(docs, removeNumbers)
#remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#Remove your own stop word
#specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1","blabla2"))
#Remove punctuation
docs <- tm_map(docs, removePunctuation)
#Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Converting to TDM/DTM
dtm <- TermDocumentMatrix(docs) 
inspect(dtm)
a <- as.matrix(dtm)
?rowSums
b <- sort(rowSums(a),decreasing = TRUE)
d <- data.frame(word = names(b), freq=b)
View(d)

poswords <- scan('positive-words.txt', what='character', comment.char = ";")
negwords <- scan('negative-words.txt', what='character', comment.char = ";")
# 
# install.packages("stringr")
# library(stringr)
# docs_1 <- docs
# inspect(docs_1)
# bag_of_words <- str_split(docs_1, pattern="\\s")
# class(bag_of_words)
# View(bag_of_words)
# match(bag_of_words, poswords)



# Wordcloud generation
wordcloud(words = d$word, freq = d$freq, min.freq = 0, max.words=1000, random.order=FALSE, rot.per=0.35, colors=brewer.pal(1, "Dark2"))
findFreqTerms(dtm, lowfreq = 8)
findAssocs(dtm, term = "freedom", corlimit = 0.3)
head(d,10) #top 10 words
tail(d,10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col = "lightblue", main = "Most frequent words", ylab = "Word frequencies")
