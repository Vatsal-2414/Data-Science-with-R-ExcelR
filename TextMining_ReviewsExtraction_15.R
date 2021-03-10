# Text mining - Extraction of amazon reviews
# Session 15

setwd('/Users/vatsalmandalia')
library(rvest)
install.packages("XML")
library(XML)
library(magrittr)

#Amazon Reviews ################
aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-MQD32HN/dp/B073Q5R6VR/ref=sr_1_1_sspa?crid=3N5NLYD0RICED&keywords=macbook+air+13+inch+laptop&qid=1582094056&sprefix=macbook+air+13+inch+%2Caps%2C284&sr=8-1-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUExTlpLWE1UV0FNU0tPJmVuY3J5cHRlZElkPUExMDQzMzIyQTJSNTA5MjJNV0JMJmVuY3J5cHRlZEFkSWQ9QTA5NTUyODcyVFNRSkZVUEVaMEU2JndpZGdldE5hbWU9c3BfYXRmJmFjdGlvbj1jbGlja1JlZGlyZWN0JmRvTm90TG9nQ2xpY2s9dHJ1ZQ==#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))   #Use html()
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"vatsalmandalia_1.txt",row.names = F)
getwd()
