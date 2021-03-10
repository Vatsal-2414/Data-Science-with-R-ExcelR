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
    html_nodes("#tn15content p") %>%
    html_text()
  movie_reviews <- c(movie_reviews, mrev)
}
movie_reviews
write.csv(movie_reviews,file = "Movie_reviews_Aendgame.csv")
write.table(movie_reviews, "AvengersEndgame.txt")

thanos <- read.csv(file.choose())
View(thanos)
html_text(html_text(html_node("#tn15content p",murl)))