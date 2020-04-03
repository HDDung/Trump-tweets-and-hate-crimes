library(rtweet)
library(tidyverse)
library(tidytext)
library(ggmap)
library(igraph)
library(ggraph)
library(twitteR)

setup_twitter_oauth(consumer_key    = "27XDRcmLg3SF42NU5asIBGbAR", 
                    consumer_secret  = "1mJzCkSDH4OrAJzpN23bUikvCIggaXlW5c1hqXcF4XALIO0pF6", 
                    access_token     = "1165639646136754182-CG5BmuUOGcZCl6hmc2nIFo6ADQrSeG",
                    access_secret    = "mHHJNsbeY76UTUDUcAPzAnyQIsIUAf7NDutyTWusp16ul")
create_token(
  app = "fcdd-course",
  consumer_key = "27XDRcmLg3SF42NU5asIBGbAR",
  consumer_secret = "1mJzCkSDH4OrAJzpN23bUikvCIggaXlW5c1hqXcF4XALIO0pF6",
  access_token = "1165639646136754182-CG5BmuUOGcZCl6hmc2nIFo6ADQrSeG",
  access_secret = "mHHJNsbeY76UTUDUcAPzAnyQIsIUAf7NDutyTWusp16ul"
)




corona_tweets <- searchTwitter('#ChineseVirus lang:en', 
                               since='2020-03-01', 
                               until='2020-04-01')


cnn_flw <- get_followers("realDonaldTrump", n = 20)









##https://www.tidytextmining.com/sentiment.html#sentiment-analysis-with-inner-join



get_token()



## Get Trump tweets

djt <- get_timeline("realDonaldTrump", n = 3200)
trump <- getUser("realDonaldTrump")



searchTwitter('charlie sheen', since='2011-03-01', until='2011-03-02')


corona_tweets <- search_tweets('#ChineseVirus lang:en unit:2020-03-18', n = 100)
length(unique(corona_tweets$user_id))


corona_tweets$text <- gsub("http.*", "", corona_tweets$text)
corona_tweets$text <- gsub("https.*", "", corona_tweets$text)
corona_tweets$text <- gsub("&amp;", "&", corona_tweets$text)


corona_tweets_clean <- corona_tweets %>%
  select(user_id, text) %>%
  unnest_tokens(word, text) %>%
  full_join(corona_tweets, by = "user_id")

length(unique(corona_tweets_clean$user_id))

stopwds <- get_stopwords("en")
keywords_cleaner <- corona_tweets_clean %>%
  anti_join(stopwds)

length(unique(keywords_cleaner$user_id))



corona_tweets_attitude_afinn <- keywords_cleaner %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(user_id) %>%
  summarise(attitude = mean(value)) 

length(unique(corona_tweets_attitude_afinn$user_id))
View(corona_tweets_attitude_afinn)
hist(corona_tweets_attitude_afinn$attitude)


corona_tweets_attitude_bing <- keywords_cleaner %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::select(user_id, word, text, sentiment) %>%
  count(user_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

View(corona_tweets_attitude_bing)
hist(corona_tweets_attitude_bing$sentiment)







keywords_cleaner %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(x = word, y = n)) +
  coord_flip()


bigrams <- corona_tweets %>%
  select(text) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwds$word) %>%
  filter(!word2 %in% stopwds$word)

bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 10)


bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_colour = n)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1.5)

