library(tidyverse)
library(readxl)
library(readr)
library(tidytext)
library(rtweet)


keywords <- c('border', 'criminal', 'drug', 'gang', 'human trafficking',
              'thugs', 'immigration', 'Mexico', 'Wall', 'Make America')

Trump_tweet <- read_delim(
  "processed-data/Trump_tweet.csv",
  "|",
  escape_double = FALSE,
  trim_ws = TRUE
)

Trump_tweet$text <- gsub("http.*", "", Trump_tweet$text)
Trump_tweet$text <- gsub("https.*", "", Trump_tweet$text)
Trump_tweet$text <- gsub("&amp;", "&", Trump_tweet$text)
Trump_tweet$created_at <- as.Date(Trump_tweet$created_at, format = '%m-%d-%Y')

freq_neg_tweet <- Trump_tweet %>%
  mutate(negative = str_count(text, paste(keywords, collapse="|"))) %>%
  mutate(week = cut.Date(created_at, breaks = "1 week", labels = FALSE)) %>%
  group_by(week) %>%
  summarise(freq = sum(negative > 0)/n(),
            total_negative = sum(negative > 0),
            total_tweet = n())

write.csv(freq_neg_tweet, file = 'processed-data/freq_neg_trump.csv')



