## Fixed effects regressions 

######################################################################################################

############# Effects of Trump's tweets freq on the freq of users' tweets Immigration ##################

freq_sentiment_immi <- 
  usa_tweets_immigration %>%
  select(date, 
         text_clean, 
         SentimentGI, 
         state) %>%
  arrange(date) %>%
  group_by(date, state) %>%
  count() %>%
  view()
         


freq_trump_daily <- Trump_tweet_daily
freq_trump_daily$state <- "CA"
for (state in unique(freq_sentiment_immi$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_daily
    tmp$state <- state
    freq_trump_daily <- rbind(freq_trump_daily, tmp)
  }
}


df_sentiment_daily <- freq_trump_daily %>% 
  left_join(freq_sentiment_immi, by=c("created_at" = "date", 'state' = 'state')) %>%
  view()


fit_sentiment <- plm(n ~ freq , data=df_sentiment_daily, 
                     index = c("state", "created_at"),
                     model = "within",
                     effect = "individual")


coeftest(fit_sentiment, vcov=vcovHC(fit_sentiment, type="sss", cluster="group")) 



#####################################################################################################

###### The frequency of Trump's tweets per day and the level of negative sentiment per day ##########


 sentiment_tweets_immi_daily <- 
  usa_tweets_immigration %>% 
  select(date, 
         text_clean, 
         SentimentGI, 
         state) %>%
  arrange(date) %>%
  group_by(date, state) %>%
  summarise(SentimentGI_mean = mean(SentimentGI))


freq_trump_daily <- Trump_tweet_daily
freq_trump_daily$state <- "CA"
for (state in unique(sentiment_tweets_immi_daily$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_daily
    tmp$state <- state
    freq_trump_daily <- rbind(freq_trump_daily, tmp)
  }
}



df_sentiment_daily_sen_neg <- freq_trump_daily %>% 
  left_join(sentiment_tweets_immi_daily, by=c("created_at" = "date", 'state' = 'state')) %>%
  replace_na(list('SentimentGI_mean' = 0))



## Running the fixed effects regression
fit_sentiment_neg <- plm(SentimentGI_mean ~ freq , data=df_sentiment_daily_sen_neg, 
           index = c("state", "created_at"),
           model = "within",
           effect = "individual")


coeftest(fit_sentiment_neg, vcov=vcovHC(fit_sentiment_neg, type="sss", cluster="group")) 


###################### Trump's tweets freq on users' tweets frequency Coronavirus #####################

freq_sentiment_covid <- 
  usa_tweets_coronavirus %>%
  select(date, 
         text_clean, 
         SentimentGI, 
         state) %>%
  arrange(date) %>%
  group_by(date, state) %>%
  count() %>%
  view()



freq_trump_daily <- Trump_tweet_daily
freq_trump_daily$state <- "WI"
for (state in unique(freq_sentiment_covid$state)){
  if (state != "WI"){
    tmp <- Trump_tweet_daily
    tmp$state <- state
    freq_trump_daily <- rbind(freq_trump_daily, tmp)
  }
}


df_sentiment_daily_sen_covid <- freq_trump_daily %>% 
  left_join(freq_sentiment_covid, by=c("created_at" = "date", 'state' = 'state')) %>%
  view()


## Running the fixed effects regression
fit_sentiment_covid <- plm(n ~ freq , data=df_sentiment_daily_sen_covid, 
                     index = c("state", "created_at"),
                     model = "within",
                     effect = "individual")


coeftest(fit_sentiment_covid, vcov=vcovHC(fit_sentiment_covid, type="sss", cluster="group")) 


############## Trump's tweets freq and the level of negative sentiment per day for Covid #############


sentiment_tweets_covid_daily <- 
  usa_tweets_coronavirus %>% 
  select(date, 
         text_clean, 
         SentimentGI, 
         state) %>%
  arrange(date) %>%
  group_by(date, state) %>%
  summarise(SentimentGI_mean = mean(SentimentGI))


freq_trump_daily <- Trump_tweet_daily
freq_trump_daily$state <- "CA"
for (state in unique(sentiment_tweets_covid_daily$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_daily
    tmp$state <- state
    freq_trump_daily <- rbind(freq_trump_daily, tmp)
  }
}



df_sentiment_daily_sen_neg_covid <- freq_trump_daily %>% 
  left_join(sentiment_tweets_covid_daily, by=c("created_at" = "date", 'state' = 'state')) %>%
  replace_na(list('SentimentGI_mean' = 0))



## Running the fixed effects regression
fit_sentiment_neg <- plm(SentimentGI_mean ~ freq , data=df_sentiment_daily_sen_neg, 
                         index = c("state", "created_at"),
                         model = "within",
                         effect = "individual")


coeftest(fit_sentiment_neg, vcov=vcovHC(fit_sentiment_neg, type="sss", cluster="group")) 









