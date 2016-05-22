## Collect tweets from Twitter

## install necessary packages
install.packages("twitteR")
library(twitteR)

##Authentication

#Replace the XXX with your values
consumerKey <- "SflHiwscs2Zlinxn2JFYdY0Js"
consumerSecret <- "vEoEYUlgRLnCTYWXVn8LzrtmijuYVqfn0PK9dHOXWiyekh23Ab"

#Replace with your access_token and access_secret
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)

# Stream tweets ( @prioridata as an example)
tweets <- userTimeline("prioridata", n = 3200, includeRts = TRUE, excludeReplies = FALSE)

# tweets number ( here we got 1709 tweets & retweets)
(n.tweet <- length(tweets))

# convert tweets to dataframe
tweets.df <- twListToDF(tweets)
# dim = 16 variables
dim(tweets.df)

# get the 1st, 2nd & last tweets and write them
for (i in c(1:2, 1709)) {
    cat(paste0("[", i, "] "))
    writeLines(strwrap(tweets.df$text[i], 60))
}

## Cleaning tweets text
library(tm)
myCleanedText <- Corpus(VectorSource(tweets.df$text))

# convert to lower case
myCleanedText <- tm_map(myCleanedText, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCleanedText <- tm_map(myCleanedText, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCleanedText <- tm_map(myCleanedText, content_transformer(removeNumPunct))


# add three extra stop words
myStopwords <- c(stopwords('english'), "available", "via", "the")

# print stop words
myStopwords

# you can remove some stop words of your choice
myStopwords <- setdiff(myStopwords, c("available"))

# remove stopwords from corpus
myCleanedText <- tm_map(myCleanedText, removeWords, myStopwords)

# remove extra whitespace
myCleanedText <- tm_map(myCleanedText, stripWhitespace)

# keep a copy of the cleaned tweets to use later as a dictionary for stem completion
myCleanedTextCopy <- myCleanedText

# stem words
install.packages("SnowballC")
library(SnowballC)
myCleanedText <- tm_map(myCleanedText, stemDocument)

###
# inspect the first 5 documents (tweets)
# inspect(myCleanedText[1:5])
# The code below is used for to make text fit for paper width
for (i in c(1:2, 1358)) {
    cat(paste0("[", i, "] "))
    writeLines(strwrap(as.character(myCleanedText[[i]]), 60))
}

# Completing dictionary
stemCompletion2 <- function(x, dictionary) {
    x <- unlist(strsplit(as.character(x), " "))
    # Unexpectedly, stemCompletion completes an empty string to
    # a word in dictionary. Remove empty string to avoid above issue.
    x <- x[x != ""]
    x <- stemCompletion(x, dictionary = dictionary)
    x <- paste(x, sep = "", collapse = " ")
    PlainTextDocument(stripWhitespace(x))
}
myCleanedText <- lapply(myCleanedText, stemCompletion2, dictionary = myCleanedTextCopy)
myCleanedText <- Corpus(VectorSource(myCleanedText))

## few examples
# count frequency of "data" = 144
miningCases <- lapply(myCleanedTextCopy,
function(x) {
    grep(as.character(x), pattern = "\\<data")
})
sum(unlist(miningCases))

# count frequency of "mobile" = 268
miningCases <- lapply(myCleanedTextCopy,
function(x) {
    grep(as.character(x), pattern = "\\<mobile")
})
sum(unlist(miningCases))

# Term Document Matrix
tdm <- TermDocumentMatrix(myCleanedText,
control = list(wordLengths = c(1, Inf)))
tdm
## Output of tdm
#< < TermDocumentMatrix(terms:4725, documents:1709) > >
#Non - / sparse entries:19758 / 8055267
#Sparsity:100 %
#Maximal term length:30
#Weighting:term frequency(tf)
##

idx <- which(dimnames(tdm)$Terms == "apps")
inspect(tdm[idx + (0:10), 10:21])

(freq.terms <- findFreqTerms(tdm, lowfreq = 50))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
xlab("Terms") + ylab("Count") + coord_flip()

## association
# which words are associated with 'data'?
findAssocs(tdm, "data", 0.2)

# which words are associated with 'app'?
findAssocs(tdm, "app", 0.15)

## Install graph libraries and plotting

library(devtools)
install_github("cran/Rgraphviz")
install_github("cran/graph")
install_github("cran/BiocInstaller")

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

library(graph)
library(Rgraphviz)

plot(tdm, term = (500:510), corThreshold = 0.1, weighting = T)


## Word Cloud
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
install.packages("RColorBrewer")
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")
pal <- pal[ - (1:4)]
# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, max.words = 30,
random.order = F, colors = pal)


##Clustering

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward")
plot(fit)
# cut dendrogram into 6 clusters
rect.hclust(fit, k = 6)


m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers
for (i in 1:k) {
    cat(paste("cluster ", i, ": ", sep = ""))
    s <- sort(kmeansResult$centers[i,], decreasing = T)
    cat(names(s)[1:5], "\n")
    # print the tweets of every cluster
    # print(tweets[which(kmeansResult£cluster==i)])
}

plot(names(s)[1:5])

## topic modeling

dtm <- as.DocumentTermMatrix(tdm)
install.packages("topicmodels")
library(topicmodels)

lda <- LDA(dtm, k = 8) # find 8 topics
(term <- terms(lda, 6)) # first 6 terms of every topic


# first topic identified for every document (tweet)
topic <- topics(lda, 1)
topics <- data.frame(date = as.Date(tweets.df$created), topic)
qplot(date, ..count.., data = topics, geom = "density",
fill = term[topic], color = term[topic], position = "stack")

## sentimental analysis
install_github("cran/plyr")
install.packages("sentiment")
library(sentiment)
install.packages("plyr")
library(plyr)

#prepare text
# get the text
some_txt = sapply(tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x) {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
        y = tolower(x)
    # result
    return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

#Sentiment Analysis
# classify emotion
class_emo = classify_emotion(some_txt, algorithm = "bayes", prior = 1.0)
# get emotion best fit
emotion = class_emo[, 7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm = "bayes")
# get polarity best fit
polarity = class_pol[, 4]
## download deprecated sentiment package 
if (!require("pacman"))
    install.packages("pacman")
pacman::p_load(devtools, installr)
install.Rtools()
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

# data frame with results
sent_df = data.frame(text = some_txt, emotion = emotion,
polarity = polarity, stringsAsFactors = FALSE)

# sort the data frame
sent_df = within(sent_df,
  emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing = TRUE))))

sent_df
# plot distribution of emotions
library(ggplot2)
ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "emotion categories", y = "number of tweets")

# plot distribution of polarity
ggplot(sent_df, aes(x = polarity)) +
  geom_bar(aes(y = ..count.., fill = polarity)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(x = "polarity categories", y = "number of tweets")

##cloud word seperated by emotions
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo) {
    tmp = some_txt[emotion == emos[i]]
    emo.docs[i] = paste(tmp, collapse = " ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
     scale = c(3, .5), random.order = FALSE, title.size = 1.5)