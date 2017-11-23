# remove all variables; start with a clean slate
rm(list=ls(all=TRUE))

# load libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(twitteR)
library(tm)
library(wordcloud)

# Connect to Twitter
ckey <- ""
skey <- ""
token <- ""
stoken <- ""
setup_twitter_oauth(ckey, skey, token, stoken)

# search twitter
term <- "cape town"
tweets <- searchTwitter(term, n=5000, lang='en')
tweets <- sapply(tweets, function(x) x$getText())

# clean text data
text <- iconv(tweets, 'UTF-8', 'ASCII')
corpus <- Corpus(VectorSource(text))

# create a term matrix
terms <- TermDocumentMatrix(corpus,
                            control = list(
                              removePunctuation=TRUE,
                              stopwords=c(term, "https", "town", "cape", "http", stopwords('english')),
                              removeNumbers=TRUE,
                              tolower=TRUE))
terms <- as.matrix(terms)

# get word counts
word_freq <- sort(rowSums(terms), decreasing=T)
df <- data.frame(word=names(word_freq), freq=word_freq)

wordcloud(df$word, df$freq, random.order=F)