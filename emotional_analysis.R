######### Emotions #############
library(lexicon)
library(tidytext)
library(dplyr)
library(radarchart)

# read in tweets from @realDonalTrump - limited to 1/20/17 to 3/29/18
# tweets from time as president
df <- read.csv("trump_tweets_012017_032918_tweets_only.csv", header=TRUE,
               stringsAsFactors=FALSE)

df$created_at <- as.Date(df$created_at, "%m-%d-%Y")

df <- df[order(df$created_at, decreasing = FALSE),]

df$id <- seq.int(nrow(df))

# convert from utf-8 to ascii
df$ascii_text <- iconv(df$text, from = "UTF-8", to = "ASCII", sub = "")

# making a corpus of a vector source and pre_processing
mytext_corpus <- VCorpus(VectorSource(df$text))
print(mytext_corpus[[35]][1])

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

cleaned_mytext_corpus <- clean_corpus(mytext_corpus)
print(cleaned_mytext_corpus[[35]][1])

# make term document matrix
TDM_mytext <- TermDocumentMatrix(cleaned_mytext_corpus)

# convert to tidy format
mytext_tidy <- tidy(TDM_mytext)

# emotion radar chart with NRC lexicon
nrc_lex <- get_sentiments("nrc")
table(nrc_lex$sentiment)
story_nrc <- inner_join(mytext_tidy, nrc_lex, by = c("term" = "word"))
story_nrc_noposneg <- story_nrc[!(story_nrc$sentiment %in% c("positive",
                                                             "negative")),]
nrc_aggdata <- aggregate(story_nrc_noposneg$count, 
                     list(index = story_nrc_noposneg$sentiment), sum)
chartJSRadar(nrc_aggdata)

# emotion radar chart with loughran lexicon
loughran_lex <- get_sentiments("loughran")
table(loughran_lex$sentiment)
story_loughran_lex <- inner_join(mytext_tidy, 
                                 loughran_lex, by = c("term" = "word"))
story_loughran_lex_noposneg <- 
        story_loughran_lex[!(story_loughran_lex$sentiment %in% c("positive",
                                                                 "negative")),]
lou_aggdata <- aggregate(story_loughran_lex_noposneg$count, 
                     list(index = story_loughran_lex_noposneg$sentiment), sum)
chartJSRadar(lou_aggdata)

