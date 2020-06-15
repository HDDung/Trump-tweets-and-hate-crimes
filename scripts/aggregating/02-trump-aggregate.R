library(tidyverse)
library(readxl)
library(readr)
library(tidytext)
library(rtweet)


keywords_hispanic <- c('border', 'criminal', 'drug', 'gang', 'human trafficking',
              'thugs', 'immigration', 'Mexico', 'Wall', 'Make America')

keywords_chinese <- c("China virus",
                      "China",
                      "Chinese virus",
                      "Coronavirus",
                      "Covid",
                      "Pandemic",
                      "W.H.O",
                      "World Health Organization")


Trump_tweet_hispanic <- read_delim(
  "../raw-data/Trump_tweet_hispanic.csv",
  "|",
  escape_double = FALSE,
  trim_ws = TRUE
)

Trump_tweet_hispanic$text <- gsub("http.*", "", Trump_tweet_hispanic$text)
Trump_tweet_hispanic$text <- gsub("https.*", "", Trump_tweet_hispanic$text)
Trump_tweet_hispanic$text <- gsub("&amp;", "&", Trump_tweet_hispanic$text)
Trump_tweet_hispanic$created_at <- as.Date(Trump_tweet_hispanic$created_at, format = '%m-%d-%Y')

freq_neg_tweet_hispanic <- Trump_tweet_hispanic %>%
  mutate(negative = str_count(text, paste(keywords_hispanic, collapse="|"))) %>%
  mutate(week = cut.Date(created_at, breaks = "1 week", labels = FALSE)) %>%
  group_by(week) %>%
  summarise(freq_trump = sum(negative > 0)/n(),
            total_negative_trump = sum(negative > 0),
            total_tweet_trump = n())

write.csv(freq_neg_tweet_hispanic, file = '../processed-data/freq_neg_trump_hispanic.csv')


Trump_tweet_chinese <- read_delim(
  "../raw-data/Trump_tweet_chinese.csv",
  "|",
  escape_double = FALSE,
  trim_ws = TRUE
)

Trump_tweet_chinese$text <- gsub("http.*", "", Trump_tweet_chinese$text)
Trump_tweet_chinese$text <- gsub("https.*", "", Trump_tweet_chinese$text)
Trump_tweet_chinese$text <- gsub("&amp;", "&", Trump_tweet_chinese$text)
Trump_tweet_chinese$created_at <- as.Date(Trump_tweet_chinese$created_at, format = '%m-%d-%Y')

freq_neg_tweet_chinese <- Trump_tweet_chinese %>%
  mutate(negative = str_count(text, paste(keywords_chinese, collapse="|"))) %>%
  mutate(week = cut.Date(created_at, breaks = "1 week", labels = FALSE)) %>%
  group_by(week) %>%
  summarise(freq_trump = sum(negative > 0)/n(),
            total_negative_trump = sum(negative > 0),
            total_tweet_trump = n())

write.csv(freq_neg_tweet_chinese, file = '../processed-data/freq_neg_trump_chinese.csv')



