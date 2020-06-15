#######################################################################
##### Cleaning and aggregating Internet access ######
#######################################################################
source("./scripts/aggregating/02-trump-aggregate.R")
## input: trump-tweets.RData
## output: freq_neg_trump.csv

#######################################################################
##### Cleaning and aggregating Internet access ######
#######################################################################
source("./scripts/aggregating/02-related-tweet-aggregate.R")
## input: related-tweets.csv
## output: hate-speed.RData

load("processed-data/sociodemographics_data.Rdata")

library(xml2)
library(rvest)
library(tidyr)

twitter_usage <- read_csv("raw-data/twitter_usage_yearly.csv")
twitter_usage_state <- twitter_usage %>%
  group_by(state) %>%
  summarise(mean_usage14_15 = sum(total)/n())

src_area <- read_html("https://www.infoplease.com/us/postal-information/state-abbreviations-and-state-postal-codes")
# store all tables in object "all_tables"

abb_state <- html_table(src_area, fill = TRUE)[[1]]
abb_state$state_name <- abb_state$`State/District` 
abb_state$abb <- abb_state$`Postal Code` 


freq_trump_hispanic <- c()
for (state in unique(abb_state$abb)){
  tmp <- freq_neg_tweet_hispanic
  tmp$state <- state
  freq_trump_hispanic <- rbind(freq_trump_hispanic, tmp)
}



#######################################################################
##### Data for hate-crime and trump tweet: Hispanic ######
#######################################################################

data_hatecrime_trumptweet_hispanic <-
  freq_trump_hispanic %>%
  left_join(incident_num_weekly) %>%
  left_join(abb_state, by = c("state" = "Postal Code")) %>%
  select(
    state,
    week,
    state_name,
    freq_trump,
    total_negative_trump,
    total_tweet_trump,
    incident_number
  ) %>%
  left_join(twitter_usage_state) %>%
  left_join(sociodemographics, by = c('state_name' = 'state')) %>%
  arrange(state, week)

View(data_hatecrime_trumptweet_hispanic)

write.csv(data_hatecrime_trumptweet_hispanic,
          file = "processed-data/data_hatecrime_trumptweet_hispanic.csv",
          row.names = FALSE)


#######################################################################
##### Data for hate-crime and users' tweets: Hispanic ######
#######################################################################

usa_tweets_hispanic_weekly <-
  usa_tweets_hispanic %>%
  add_row(date = as.Date("2017-01-20"))  %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  arrange(date, week) %>%
  filter(!is.na(SentimentGI)) %>%
  group_by(week, state) %>%
  summarise(
    total_tweet_usa = n(),
    total_negative_usa = sum(SentimentGI < 0),
    perc_negative_usa = sum(SentimentGI < 0) / n(),
    mean_sentiment = mean(SentimentGI, na.rm = TRUE)
  )

data_usertweet_trumptweet_hispanic <-
  freq_trump_hispanic %>%
  left_join(usa_tweets_hispanic_weekly, by = c("week", "state")) %>%
  left_join(abb_state, by = c("state" = "Postal Code")) %>%
  select(
    state,
    week,
    state_name,
    freq_trump,
    total_negative_trump,
    total_tweet_trump,
    perc_negative_usa,
    total_tweet_usa,
    total_negative_usa,
    mean_sentiment
  ) %>%
  left_join(twitter_usage_state) %>%
  left_join(sociodemographics, by = c('state_name' = 'state')) %>%
  arrange(state, week)

View(data_usertweet_trumptweet_hispanic)

write.csv(data_usertweet_trumptweet_hispanic,
          file = "processed-data/data_usertweet_trumptweet_hispanic.csv",
          row.names = FALSE)

#######################################################################
##### Data for hate-crime and users' tweets: Chinese ######
#######################################################################

freq_trump_chinese <- c()
for (state in unique(abb_state$abb)){
  tmp <- freq_neg_tweet_chinese
  tmp$state <- state
  freq_trump_chinese <- rbind(freq_trump_chinese, tmp)
}


usa_tweets_chinese_weekly <-
  usa_tweets_coronavirus %>%
  add_row(date = as.Date("2020-03-16"))  %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  arrange(date, week) %>%
  filter(!is.na(SentimentGI)) %>%
  group_by(week, state) %>%
  summarise(
    total_tweet_usa = n(),
    total_negative_usa = sum(SentimentGI < 0),
    perc_negative_usa = sum(SentimentGI < 0) / n(),
    mean_sentiment = mean(SentimentGI, na.rm = TRUE)
  )

data_usertweet_trumptweet_chinese <-
  freq_trump_chinese %>%
  left_join(usa_tweets_chinese_weekly, by = c("week", "state")) %>%
  left_join(abb_state, by = c("state" = "Postal Code")) %>%
  select(
    state,
    week,
    state_name,
    freq_trump,
    total_negative_trump,
    total_tweet_trump,
    perc_negative_usa,
    total_tweet_usa,
    total_negative_usa,
    mean_sentiment
  ) %>%
  left_join(twitter_usage_state) %>%
  left_join(sociodemographics, by = c('state_name' = 'state')) %>%
  arrange(state, week)

View(data_usertweet_trumptweet_chinese)

write.csv(data_usertweet_trumptweet_chinese,
          file = "processed-data/data_usertweet_trumptweet_chinese.csv",
          row.names = FALSE)




