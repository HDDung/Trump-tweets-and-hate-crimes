library(readr)
library(tidytext)
library(lubridate)
library(tm)
library(SentimentAnalysis)

preprocessTweet <- function(tweets){
  
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&amp;", "&", tweets$text)
  tweets$text <- gsub("@[[:alpha:]]*","", tweets$text)
  tweets$date <- as.Date(tweets$date, format = '%m/%d/%Y')

  text_corpus <- Corpus(VectorSource(tweets$text))
  text_corpus <- tm_map(text_corpus, tolower)
  text_corpus <- tm_map(text_corpus, removeWords, 
                          c("rodneydavis", "rt", "re", "amp"))
  text_corpus <- tm_map(text_corpus, removeWords, 
                        stopwords("english"))
  text_corpus <- tm_map(text_corpus, removePunctuation)
  
  text_df <- data.frame(text_clean = get("content", text_corpus), 
                        stringsAsFactors = FALSE)
  
  tweets <- cbind.data.frame(tweets, text_df)
  return(tweets)
}
sentimentAnalyt <- function(processed_tweet){
  state_tweet_sentiment <- analyzeSentiment(processed_tweet$text_clean) 
  
  
  state_tweet_sentiment <- dplyr::select(state_tweet_sentiment, 
                                         WordCount, 
                                         SentimentGI)
  
  processed_tweet <- cbind.data.frame(processed_tweet, state_tweet_sentiment)
  return(processed_tweet)
}

state_name_abb <- read_csv("raw-data/state_name_abb.csv")

### Hispanic
usa_tweets_hispanic <- c()
for (index in 1:nrow(state_name_abb)){
  file <- paste(state_name_abb$state_name[index], "output_got.csv", sep="")
  path <- paste("raw-data/Hispanic/", file, sep="")
  state_tweet <- read_csv(path)
  state_tweet <- preprocessTweet(state_tweet)
  state_tweet <- sentimentAnalyt(state_tweet)
  
  usa_tweets_hispanic <- rbind(usa_tweets_hispanic, state_tweet %>%
          select(date, text_clean, WordCount, SentimentGI) %>%
          mutate(state = state_name_abb$state_abb[index]))
}

write.csv(usa_tweets_hispanic, file = "processed-data/usa_tweets_hispanic.csv", row.names = FALSE)


### Coronavirus
usa_tweets_coronavirus <- c()
for (index in 1:nrow(state_name_abb)){
  file <- paste(state_name_abb$state_name[index], "output_got.csv", sep="")
  path <- paste("raw-data/Coronavirus/", file, sep="")
  state_tweet <- read_csv(path)
  state_tweet <- preprocessTweet(state_tweet)
  state_tweet <- sentimentAnalyt(state_tweet)
  
  usa_tweets_coronavirus <- rbind(usa_tweets_coronavirus, state_tweet %>%
                        select(date, text_clean, WordCount, SentimentGI) %>%
                        mutate(state = state_name_abb$state_abb[index]))
}

write.csv(usa_tweets_coronavirus, file = "processed-data/usa_tweets_coronavirus.csv", row.names = FALSE)

