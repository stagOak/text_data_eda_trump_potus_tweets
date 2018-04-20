# read in tweets from @realDonalTrump - limited to 1/20/17 to 3/29/18
# tweets from time as president
df <- read.csv("trump_tweets_012017_032918_tweets_only.csv", header=TRUE, 
               stringsAsFactors=FALSE)

# convert from utf-8 to ascii
df$ascii_text <- iconv(df$text, from = "UTF-8", to = "ASCII", sub = "")

library(tm)
# making a corpus of a vector source 
trump_tweet_corpus <- VCorpus(VectorSource(df$ascii_text))

# Cleaning corpus - pre_processing
clean_corpus <- function(cleaned_corpus){
        removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+",
                                                          "", x, perl=T))
        cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
        cleaned_corpus <- tm_map(cleaned_corpus, 
                                 content_transformer(replace_abbreviation))
        cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
        cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
        cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
        cleaned_corpus <- tm_map(cleaned_corpus, removeWords, 
                                 stopwords("english"))
        # available stopwords
        # stopwords::stopwords()
        custom_stop_words <- c("&amp", "amp")
        cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
        # cleaned_corpus <- tm_map(cleaned_corpus, stemDocument,
        #                          language = "english")
        cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
        return(cleaned_corpus)
}

cleaned_trump_tweet_corpus <- clean_corpus(trump_tweet_corpus)
print(trump_tweet_corpus[[16]][1])
print(cleaned_trump_tweet_corpus[[16]][1])

###########TDM/DTM########
TDM_trump_tweets <- TermDocumentMatrix(cleaned_trump_tweet_corpus)
TDM_trump_tweets_m <- as.matrix(TDM_trump_tweets)
TDM_trump_tweets_m[1:50, 1:10]
print(trump_tweet_corpus[[19]][1])
print(cleaned_trump_tweet_corpus[[19]][1])

# Term Frequency
term_frequency <- rowSums(TDM_trump_tweets_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

# View the top 20 most common words
top10 <- term_frequency[1:20]

# Plot a barchart of the 20 most common words
barplot(top10,col="darkorange",las=2)

############Word Cloud
library(wordcloud)

term_frequency[1:10]

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq=5, max.words=50, 
          random.order=TRUE, colors=brewer.pal(8, "Paired"), scale = c(7, 0.2))

##############bigrams

library(RWeka)

tokenizer <- function(x) NGramTokenizer(x,Weka_control(min=2,max=2))

bigram_tdm <- TermDocumentMatrix(cleaned_trump_tweet_corpus, 
                                 control = list(tokenize=tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)

# Term Frequency
term_frequency <- rowSums(bigram_tdm_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, dec=TRUE)

############Word Cloud
library(wordcloud)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq=5, max.words=25, 
          random.order=TRUE, colors=brewer.pal(8, "Paired"), scale = c(6, 0.2))

##############trigrams

tokenizer <- function(x) NGramTokenizer(x,Weka_control(min=3,max=3))

trigram_tdm <- TermDocumentMatrix(cleaned_trump_tweet_corpus,
                                  control = list(tokenize=tokenizer))

trigram_tdm_m <- as.matrix(trigram_tdm)

# Term Frequency
term_frequency <- rowSums(trigram_tdm_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

############Word Cloud
library(wordcloud)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=16,
          random.order=TRUE, colors=brewer.pal(8, "Paired"), scale = c(3, 0.2))

##########tf-idf weighting

tfidf_tdm <- TermDocumentMatrix(cleaned_trump_tweet_corpus, 
                                control=list(weighting=weightTfIdf))

tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Term Frequency
term_frequency <- rowSums(tfidf_tdm_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

############Word Cloud
library(wordcloud)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq=5, max.words=50, 
          colors=brewer.pal(8, "Paired"), random.order=TRUE, 
          scale = c(3, 0.2))
