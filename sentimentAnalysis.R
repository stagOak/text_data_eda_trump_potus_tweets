# read in tweets from @realDonalTrump - limited to 1/20/17 to 3/29/18
# tweets from time as president
df <- read.csv("trump_tweets_012017_032918_tweets_only.csv", header=TRUE, 
               stringsAsFactors=FALSE)

df$created_at <- as.Date(df$created_at, "%m-%d-%Y")

df <- df[order(df$created_at, decreasing = FALSE),]

df$id <- seq.int(nrow(df))

# convert from utf-8 to ascii
df$ascii_text <- iconv(df$text, from = "UTF-8", to = "ASCII", sub = "")

library(qdap)
(sentiment_text <- polarity(df$ascii_text))

# counts
(text_counts <- counts(sentiment_text))
text_counts$index <- seq.int(nrow(text_counts))
text_counts$created_at <- df$created_at

library(ggplot2)
p <- ggplot(text_counts, aes(created_at, polarity)) + geom_point()
p <- ggplot(text_counts, aes(created_at, polarity)) + geom_smooth()
print(p)

# commonality cloud
pos_polarity <- text_counts[text_counts$polarity >= 0, 'text.var']
pos_polarity <- paste(pos_polarity, collapse="") 
neg_polarity <- text_counts[text_counts$polarity < 0,  'text.var']
neg_polarity <- paste(neg_polarity, collapse="") 

library(qdap)
library(readtext)
library(tm)
tweets <- c(pos_polarity, neg_polarity)

#making a corpus of a vector source 
tweet_corpus <- VCorpus(VectorSource(tweets))

# pre_processing 
clean_corpus <- function(corpus){
        removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+",
                                                          "", x, perl=T))
        cleaned_corpus <- tm_map(corpus, removeURL)
        cleaned_corpus <- tm_map(corpus, 
                                 content_transformer(replace_abbreviation))
        cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
        cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
        cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
        cleaned_corpus <- tm_map(cleaned_corpus, removeWords, 
                                 stopwords("english"))
        custom_stop_words <- c("amp")
        cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
        #   cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)
        cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
        return(cleaned_corpus)
}

cleaned_tweet_corpus <- clean_corpus(tweet_corpus)

########### TDM/DTM########
TDM_tweet <- TermDocumentMatrix(cleaned_tweet_corpus)
TDM_tweet_m <- as.matrix(TDM_tweet)

#############################Commonality Cloud
library(wordcloud)
commonality.cloud(TDM_tweet_m, colors=brewer.pal(8, "Dark2"), max.words = 50)

# comparison cloud
TDM_tweets <- TermDocumentMatrix(cleaned_tweet_corpus)
colnames(TDM_tweets) <- c("positive sentiment","negative sentiment")
TDM_tweets_m <- as.matrix(TDM_tweets)
comparison.cloud(TDM_tweets_m,colors=brewer.pal(8, "Dark2"),max.words = 50)
