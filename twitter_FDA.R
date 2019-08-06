rm( list=ls() ) 
library(twitteR); 
library(tm);
library(SnowballC);
library(wordcloud);
library(ggplot2);
library(graph)
library(Rgraphviz)
setwd("C:/Personal/Cool_Analytics/Twitter_mining/FDAMedWatch");

# useful link to get going in case stuck with authentication:  https://github.com/hadley/httr/blob/master/demo/oauth1-twitter.r

ckey<-"nCh7wmA9yb9VEClzKFaReVJkM" 
csecret<-"Qx5Md1rQmOIj2tUdVP6U3JwgvfTCvCzj5lq6s1OgE5ot5pofAp"
acctoken<-"EhJY8x6SxLHJZJBrj0nfYPIgHfzeXHF13weBt9R"
accsecret<-"tHr0cqj6YpD9UFlo76T0LiShMYPU9lWJG6J4gvxoJlU3k"
setup_twitter_oauth(ckey, csecret)
feeds<-userTimeline("FDAMedWatch", n = 3200)

#feeds<-searchTwitter("farmer suicides in india", n=800)
tweets.df <- twListToDF(feeds);
write.table(tweets.df, "FDAMedWatch-3200.csv", sep=",",row.names=FALSE, quote=FALSE);
#simple attempt to create a corpus and tdm out of same works. 
myCorpus <- Corpus(VectorSource(tweets.df$text))
tdm <- TermDocumentMatrix(myCorpus,control = list(removePunctuation = TRUE,stopwords = TRUE)) 
wordcloud(myCorpus)
freq.terms <- findFreqTerms(tdm, lowfreq = 15)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
#find words associated with farmer
farmer_assoc<-findAssocs(tdm, "farmer", 0.2)

plot(tdm, term = freq.terms, corThreshold = 0.2, weighting = T)

#CLUSTERING 
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward")
plot(fit)
rect.hclust(fit, k = 6)

#TOPIC MODELLING
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 8) # find 8 topics
(term <- terms(lda, 6)) # first 6 terms of every topic
term <- apply(term, MARGIN = 2, paste, collapse = ", ")

topic <- topics(lda, 1)
topics <- data.frame(date=(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
