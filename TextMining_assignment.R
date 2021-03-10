# Text mining assignment
# Problem statement:
# ONE:
#     1) Extract tweets for any user (try choosing a user who has more tweets)
#     2) Perform sentimental analysis on the tweets extracted from the above
# TWO:
#     1) Extract reviews of any product from ecommerce website like snapdeal and amazon
#     2) Perform sentimental analysis
# THREE:
#     1) Extract movie reviews for any movie from IMDB and perform sentimental analysis
#     2) Extract anything you choose from the internet and do some research on how we extract using R
#        Programming and perform sentimental analysis.

#-------------x-------------#
# ONE: Extraction of tweets of Amitabh Bachchan (@SrBachchan)
#      Followed by sentimental analysis on the tweets data
  
library(twitteR)
library(ROAuth)

# The consumer key and consumer secret key have been used from the standard R codes
# in LMS. 
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")
library(base64enc)
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret


# Extraction of tweets
tweets <- userTimeline('SrBachchan', n=1000, includeRts = T)
?userTimeline
TweetsDF <- twListToDF(tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "SrBachchan_tweets.csv", row.names = F)
tweets_data <- read.csv(file.choose())
View(tweets_data)

text_data <- as.character(tweets_data$text)

#----x----#
# Wordcloud generation
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(tm)

# Creating a corpus
docs_tweets <- Corpus(VectorSource(text_data))
inspect(docs_tweets)

# Pre-processing the unstructured tweets data
tospace <- function(x, pattern) gsub(pattern, " ", x)
docs_tweets <- tm_map(docs_tweets, tospace, "/")
docs_tweets <- tm_map(docs_tweets, tospace, "@")
docs_tweets <- tm_map(docs_tweets, tospace, "\\|")

# Convert to lower case
docs_tweets <- tm_map(docs_tweets, tolower)

# Removing numbers
docs_tweets <- tm_map(docs_tweets, removeNumbers)

# Removing punctuation
docs_tweets <- tm_map(docs_tweets, removePunctuation)

# Removing stopwords
docs_tweets <- tm_map(docs_tweets, removeWords, stopwords("english"))
docs_tweets <- tm_map(docs_tweets, removeWords, c("blabla1","blabla2"))
docs_tweets <- tm_map(docs_tweets, removeWords, c("https","tco","..."))
my_stopwords <- readLines(file.choose())
docs_tweets <- tm_map(docs_tweets, removeWords, my_stopwords)

# Removing extra white space
docs_tweets <- tm_map(docs_tweets, stripWhitespace)


# Converting to TDM
tdm_tw <- TermDocumentMatrix(docs_tweets)
inspect(tdm_tw)
a_tw <- as.matrix(tdm_tw)
b_tw <- sort(rowSums(a_tw), decreasing = TRUE)
D_tw <- data.frame(word = names(b_tw), freq = b_tw)
View(D_tw)

# Wordcloud generation
wordcloud(words = D_tw$word, freq = D_tw$freq, min.freq = 0, max.words = 1500, random.order = FALSE, colors = brewer.pal(3, "Dark2"), rot.per = 0.5)
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = D_tw, size = 1.0)

findFreqTerms(tdm_tw, lowfreq = 10)
findAssocs(tdm_tw, term = "award", corlimit = 0.3)
head(D_tw, 10)
tail(D_tw, 10)
margin <- par(mar = c(8,4,4,2))
barplot(D_tw[1:20,]$freq, names.arg = D_tw[1:20,]$word, las = 2, col = "blue", main = "Most Frequent Words", ylab = "Word Frequencies")

#----x----#
# NLP
# Topic-wise extraction (LDA)
library(slam)
library(topicmodels)

# Document Term Matrix(DTM)
dtm_tw <- t(tdm_tw)
inspect(dtm_tw)
dim(dtm_tw)
rowTot <- apply(dtm_tw,1,sum)
dtm_tw.new <- dtm_tw[rowTot>0,]
inspect(dtm_tw.new)

# LDA
lda_tw <- LDA(dtm_tw.new,15)
term <- terms(lda_tw,10)
topics_tw <- terms(lda_tw)
tb_tw <- table(names(topics_tw), unlist(topics_tw))
tb_tw <- as.data.frame.matrix(tb_tw)
View(tb_tw)

# Clustering - to find similarities between the topics
clus_tw <- hclust(dist(tb_tw), method = 'ward.D2')
?hclust
par(family = 'HiraKakuProN-W3')
plot(clus_tw)

#----x----#
# Emotion mining/Sentiment analysis
library(syuzhet)
head(text_data)
s_v <- get_sentences(text_data)
str(s_v)

# Lexicon bing
sentiment_vec_bing <- get_sentiment(s_v, method = "bing")
head(sentiment_vec_bing)
sum(sentiment_vec_bing)
mean(sentiment_vec_bing)
summary(sentiment_vec_bing)

# Lexicon nrc
sentiment_vec_nrc <- get_sentiment(s_v, method = "nrc")
head(sentiment_vec_nrc)


plot(sentiment_vec_nrc[1:100], type = 'l', main = "Plot Trajectory (lexicon nrc)", xlab = "Tweets", ylab = "Emotional Valence")

# Most positive sentence
positive_sen <- s_v[which.max(sentiment_vec_nrc)]
# Most negative sentence
negative_sen <- s_v[which.min(sentiment_vec_nrc)]

# Percentage based plots
text_tw <- text_data

text_sen <- get_sentiment(text_tw, method = "nrc")
head(text_sen)
plot(text_sen, type = 'h', main = "LOTR using transformed values", xlab = "Tweets", ylab = "Emotional Valence")

percent_values <- get_percentage_values(text_sen)
head(percent_values)
plot(percent_values, type = 'l', main = "Percentage values", xlab = "Tweets", ylab = "Emotional Valence")

# Transformed values
transform_values <- get_transformed_values(text_sen)
head(transform_values)
plot(transform_values, type = 'h', main = "Transformed values", xlab = "Tweets", ylab = "Emotional Valence")


nrc_data <- get_nrc_sentiment(s_v)
nrc_score_word <- get_nrc_sentiment('happy')
nrc_score_word

# Emotion arcs
sad_items <- which(nrc_data$sadness>0)
head(s_v[sad_items])

margin <- par(mar = c(6,6,4,2))
barplot(sort(prop.table(colSums(nrc_data[,1:8]))), horiz = T, cex.names = 1, las = 1, main = "Emotion arc", xlab = "Percentage", col = 1:8)
# The emotion arc indicates that most of the tweets by Amitabh Bachchan show positive
# sentiment when we look at the percentages.

##-------------------x---------------------##

# TWO: URL of customer reviews webpage for Apple iPhone 6s (32GB) Rose Gold
# https://www.amazon.in/Apple-iPhone-6S-Rose-Storage/dp/B01LXF3SP9/ref=lp_20683047031_1_12?s=electronics&ie=UTF8&qid=1588860136&sr=1-12#customerReviews

setwd('/Users/vatsalmandalia')
library(rvest)
library(XML)
library(magrittr)

# 1) Extraction of reviews
aurl <- "https://www.amazon.in/Apple-iPhone-6S-Rose-Storage/dp/B01LXF3SP9/ref=lp_20683047031_1_12?s=electronics&ie=UTF8&qid=1588860136&sr=1-12#customerReviews"
amazon_reviews <- NULL
for(i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep = "=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
amazon_reviews
write.table(amazon_reviews,"vatsal_vm_amazonreviews.txt", row.names = F)

reviews <- readLines(file.choose())

#-----x------#

# 2) Text mining and Wordcloud generation
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(tm)

class(reviews)

docs <- Corpus(VectorSource(reviews))
inspect(docs)

# Pre-processing the unstructured data
# Function which replaces the matched pattern with a space
tospace <- function(x,pattern) gsub(pattern," ",x)
docs <- tm_map(docs, tospace, "/")
docs <- tm_map(docs, tospace, "@")
docs <- tm_map(docs, tospace, "\\|")
# converting to lowercase
docs <- tm_map(docs, tolower)
# removing numbers
docs <- tm_map(docs, removeNumbers)
# removing punctuation
docs <- tm_map(docs, removePunctuation)
# removing stopwords
docs <- tm_map(docs, removeWords, c("english"))
docs <- tm_map(docs, removeWords, c("blabla1","blabla2"))
my_stopwords <- readLines(file.choose())
docs <- tm_map(docs, removeWords, my_stopwords)
# removing extra white space
docs <- tm_map(docs, stripWhitespace)

# Converting to a DTM/TDM
tdm <- TermDocumentMatrix(docs)
inspect(tdm)
a <- as.matrix(tdm)
b <- sort(rowSums(a), decreasing = TRUE)
D <- data.frame(word = names(b), freq = b)
View(D)

# Wordcloud generation
wordcloud(words = D$word, freq = D$freq, min.freq = 0, max.words = 1000, random.order = FALSE, colors = brewer.pal(2,"Dark2"), rot.per = 0.5)
findFreqTerms(tdm, lowfreq = 10)
findAssocs(tdm, terms = "camera", corlimit = 0.4)
head(D,10)
tail(D,10)

margin <- par(mar = c(6,4,4,2))
barplot(D[1:30,]$freq, width = 0.5, names.arg = D[1:30,]$word, col = "skyblue",las = 2, main = "Most Frequent Words",ylab = "Word Frequencies")
#rm(marg)

#----x----#
# NLP
# Topic-wise extraction (LDA)
library(topicmodels)
library(slam)

# Creating a Document Term Matrix
dtm <- t(tdm)
dim(dtm)
rowTotals <- apply(dtm,1,sum)
dtm.new <- dtm[rowTotals>0,]
dim(dtm.new)
inspect(dtm.new)

# LDA
lda <- LDA(dtm.new,10) # 10 topics
term <- terms(lda,10)
topics <- terms(lda)
table_topics <- table(names(topics), unlist(topics))
table_topics <- as.data.frame.matrix(table_topics)
View(table_topics)

# Clustering - to find similarities between the topics
clus <- hclust(dist(table_topics), method = 'ward.D2')
?par
par(family = 'HiraKakuProN-W3')
plot(clus)


#----x----#
# Emotion mining/Sentiment analysis
library(syuzhet)

s_v <- get_sentences(reviews)
str(s_v)
class(s_v)

# Lexicon 'nrc'
nrc_sentiment <- get_sentiment(s_v,method = "nrc")
head(nrc_sentiment)

# Lexicon 'bing'
bing_sentiment <- get_sentiment(s_v,method = "bing")
head(bing_sentiment)
sum(bing_sentiment)
mean(bing_sentiment)
summary(bing_sentiment)

plot(bing_sentiment, type = 'l', main = "Plot Trajectory",xlab = "Reviews", ylab = "Emotional Valence")

# Most negative sentence
negative_sent <- s_v[which.min(bing_sentiment)]
# Most positive sentence
positive_sent <- s_v[which.max(bing_sentiment)]
  

# Percentage based figures
review_text <- reviews
rev_sentiment <- get_sentiment(review_text, method = "bing")
head(rev_sentiment)
plot(rev_sentiment, type = 'h', main = "LOTR using transformed values", xlab = "Reviews", ylab = "Emotional Valence")

per_values <- get_percentage_values(rev_sentiment) 
head(per_values)
plot(per_values, type = 'l', main = "Percentage_based trajectory",xlab = "Reviews", ylab = "Emotional Valence")

trans_values <- get_transformed_values(rev_sentiment, low_pass_size = 3, x_reverse_len = 100, scale_vals = TRUE, scale_range = FALSE)
head(trans_values)
plot(trans_values, type = 'h', main = "LOTR using Transformed values", xlab = "Reviews", ylab = "Emotional Valence", col = "red")

nrc_sent <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment('bad')
nrc_score_word <- get_nrc_sentiment('grim')

# Emotion arcs
# using nrc lexicon
sad_items <- which(nrc_sent$sadness>0)
head(sad_items)
head(s_v[sad_items]) # Sentences which show the emotion of sadness.
str(nrc_sent)
barplot(sort(colSums(prop.table(nrc_sent[,1:10]))), horiz = T, cex.names = 0.65, las = 1, main = "Emotions", xlab = "Percentage", col = 1:10)


#-------x--------#
# Conclusion: The amazon reviews of iPhone 6s was extracted from the url. 
#             This was followed by carrying out Wordcloud generation which gave a general
#             idea on the common words used by the customers for reviewing the item.
#             Then Natural Language Processing was done which involved performing 
#             unsupervised LDA technique. This allowed topic-wise extraction from the data.
#             Also, to identify similarities between the topics hierarchical clustering
#             was done.
#             Finally, sentiment analysis was the last method to be used. Using bing and 
#             nrc lexicons, certain plots were produced. Most of the plots showed the 
#             Emotional valence varying from one review to another. The final barplot 
#             gave a percentage for each of the 10 emotions present in the reviews.
#-----------------x------------------#

# THREE: 1) Extraction of movie reviews of Avengers Endgame (2019) from IMDB and
#        perform sentimental analysis

library(rvest)
library(magrittr)
library(XML)

# Extraction of reviews
a <- 10

movie_reviews <- NULL
aurl <- "https://www.imdb.com/title/tt4154796/reviews?ref_=tt_ov_rt"

for(i in 0:30){
  murl <- read_html(as.character(paste(aurl,i*a,sep = "")))
  mrev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  movie_reviews <- c(movie_reviews, mrev)
}
head(movie_reviews,10)
write.csv(movie_reviews,file = "Movie_reviews_Aendgame.csv")
write.table(movie_reviews, "Movie_reviews_AvengersEndgame.txt", row.names = F)

endgame_revs <- readLines(file.choose())
View(endgame_revs)

#----x----#
# Sentimental analysis
# Wordcloud generation
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

# Pre-processing the unstructured text data
docs_reviews <- Corpus(VectorSource(endgame_revs))
inspect(docs_reviews)

# Removing unnecessary words
toSpace <- function(x,pattern) gsub(pattern," ",x)
docs_reviews <- tm_map(docs_reviews, toSpace, "@")
docs_reviews <- tm_map(docs_reviews, toSpace, "/")
docs_reviews <- tm_map(docs_reviews, toSpace, "\\|")

# Converting to lowercase
docs_reviews <- tm_map(docs_reviews, tolower)

# Removing stopwords
docs_reviews <- tm_map(docs_reviews, removeWords, stopwords("english"))
docs_reviews <- tm_map(docs_reviews, removeWords, c("blabla1","blabla2"))
my_stopwords <- readLines(file.choose())
docs_reviews <- tm_map(docs_reviews, removeWords, my_stopwords)

# Removing punctuation
docs_reviews <- tm_map(docs_reviews, removePunctuation)

#Removing numbers
docs_reviews <- tm_map(docs_reviews, removeNumbers)

# Removing extra white space
docs_reviews <- tm_map(docs_reviews, stripWhitespace)

# Converting to Term Document Matrix
tdm_mr <- TermDocumentMatrix(docs_reviews)
inspect(tdm_mr)

a_mr <- as.matrix(tdm_mr)
b_mr <- sort(rowSums(a_mr), decreasing = TRUE)
D_mr <- data.frame(word = names(b_mr), freq = b_mr)

View(D_mr)

# Wordcloud generation
wordcloud(words = D_mr$word, freq = D_mr$freq, min.freq = 0, max.words = 1000, random.order = FALSE, colors = brewer.pal(3,"Dark2"))
library(wordcloud2)
wordcloud2(D_mr)
findFreqTerms(tdm_mr, lowfreq = 20)
findAssocs(tdm_mr, term = "captain", corlimit = 0.4)
head(D_mr,10)
tail(D_mr,10)

margin <- par(mar = c(6,4,4,2))
barplot(D_mr[1:10,]$freq, names.arg = D_mr[1:10,]$word, col = "gold", las = 2, main = "Most Frequent Words", ylab = "Word Frequencies")

#----x----#
# NLP
library(topicmodels)
library(slam)

# Creating a Document Term Matrix
dtm_mr <- t(tdm_mr)
dim(dtm_mr)
rowtot_mr <- apply(dtm_mr,1,sum)
dtm_mr.new <- dtm_mr[rowtot_mr>0,]
inspect(dtm_mr.new)

# LDA
lda_mr <- LDA(dtm_mr.new,10) # 10 topics
term_mr <- terms(lda_mr,10)
topics_mr <- terms(lda_mr)
table_mr <- table(names(topics_mr),unlist(topics_mr))
table_mr <- as.data.frame.matrix(table_mr)
View(table_mr)

# Clustering - to find similarities between the topics
clus_mr <- hclust(dist(table_mr), method = "ward.D2")
par(family = "HiraKakuProN-W3")
plot(clus_mr)

#----x----#
# Emotion mining
library(syuzhet)

sentence_v <- get_sentences(endgame_revs)
head(sentence_v)

# Lexicon bing
sent_vec_bing <- get_sentiment(sentence_v, method = "bing")
head(sent_vec_bing)
sum(sent_vec_bing)
mean(sent_vec_bing)
summary(sent_vec_bing)

# Lexicon nrc
sent_vec_nrc <- get_sentiment(sentence_v, method = "nrc")
head(sent_vec_nrc)

# Most positive sentence (lexicon - bing)
positive_sentence <- sentence_v[which.max(sent_vec_bing)]
positive_sentence
pos_sentence <- sentence_v[which.max(sent_vec_nrc)] # lexicon - nrc
pos_sentence

# Most negative sentence (lexicon - bing)
negative_sentence <- sentence_v[which.min(sent_vec_bing)]
negative_sentence
neg_sentence <- sentence_v[which.min(sent_vec_nrc)] # lexicon - nrc
neg_sentence

# Trajectory of the reviews
plot(sent_vec_bing, type = 'l', main = "Plot Trajectory", ylab = "Emotional Valence", xlab = "Reviews")

# Percentage based 
endgame_text <- endgame_revs
sent_vec_endgame <- get_sentiment(endgame_text, method = "bing")
plot(sent_vec_endgame, type = 'h', main = "LOTR using transformed values", xlab = "Reviews", ylab = "Emotional Valence")

percent_vals_av <- get_percentage_values(sent_vec_endgame)
plot(percent_vals_av, type = 'l', main = "Percentage based", xlab = "Reviews", ylab = "Emotional Valence")

# Transformed values
transform_vals_av <- get_transformed_values(sent_vec_endgame, low_pass_size = 3, x_reverse_len = 100, scale_vals = TRUE, scale_range = FALSE)
plot(transform_vals_av, type = 'h', main = "LOTR using transformed values", xlab = "Reviews", ylab = "Emotional Valence", col = 'red')

# Emotion arcs
nrc_data <- get_nrc_sentiment(sentence_v)
get_nrc_sentiment('smile')

sad_items <- which(nrc_data$sadness>0)
head(sentence_v[sad_items])  
margin <- par(mar = c(6,5,4,2))
barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7, las = 1, main = "Emotions", xlab = "Percentage", col = 1:8)

#----x----#
# 2) Extract any thing from the internet and do some research on how we extract using R
library(rvest)
library(XML)
library(magrittr)

# Prime Minister Narendra Modi Speech on 12/05/2020
pm_url <- "https://www.pmindia.gov.in/en/news_updates/pms-address-to-the-nation-on-12-5-2020/?tag_term=pmspeech&comment=disable"
speech_text <- NULL

speech_url <- read_html(pm_url)
text <- html_nodes(speech_url,".news-bg p") %>%
  html_text()

write.table(text,'PM_modi_speech.txt', row.names = F)
modi_speech <- readLines(file.choose())
modi_speech

#------------------Below is not to be run---------------#
# How the extraction is done using R
Web scraping is a technique for converting the data present in unstructured format
(HTML tags) over the web to the structured format allowing for text mining analysis.

The applications for web scraping are shown below:
  1. Scraping movie rating data to create movie recommendation engines.
  2. Scraping text data from Wikipedia and other sources for making NLP-based systems 
     or training deep learning models for tasks like topic recognition from the given 
     text.
  3. Scraping speeches of personalities from the web and generating wordclouds.
  4. Scraping user reviews from e-commerce websites like Amazon, Flipkart, Snapdeal etc.
  5. Scraping data from social media platforms like Facebook, Twitter etc. to perform
     Sentiment analysis and emotion mining.

There are different ways to scrape data such as Human Copy-Paste, Text Pattern Matching,
API Interface and DOM Parsing. 
1. Human Copy-Paste
   This is a slow and efficient way of scraping data from the web. This involves 
   manually copying the data on a text editor and analysing it.
2. Text Pattern Matching
   Using regular expression matching facilities of various programming languages
   information from the web can be extracted.
3. API Interface
   Many websites like Facebook, Twitter, LinkedIn, etc. provide public or private 
   APIs which can be called using the standard code for retrieving the data in the 
   prescribed format.
4. DOM Parsing
   By using web browsers, programs can retrieve the dynamic content generated by 
   client-side scripts.
   
The method used in the extraction of the PM Modi speech is DOM Parsing. We use different
R packages like rvest and XML in the extraction process.
   
   
   