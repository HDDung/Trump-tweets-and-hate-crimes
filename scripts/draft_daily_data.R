Trump_tweet_daily <- Trump_tweet %>%
  mutate(negative = str_count(text, paste(keywords, collapse="|"))) %>%
  group_by(created_at) %>%
  summarise(freq = sum(negative > 0)/n(),
            total_negative = sum(negative > 0),
            total_tweet = n())


sentiment_tweets_immi_daily <- 
  usa_tweets %>% 
  select(date, 
         text_clean, 
         SentimentGI, 
         state) %>%
  arrange(date) %>%
  group_by(date, state) %>%
  summarise(SentimentGI_mean = mean(SentimentGI))



freq_trump_daily <- Trump_tweet_daily
freq_trump_daily$state <- "CA"
for (state in unique(usa_tweets_immigration$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_daily
    tmp$state <- state
    freq_trump_daily <- rbind(freq_trump_daily, tmp)
  }
}


df_sentiment_daily <- freq_trump_daily %>% 
  left_join(sentiment_tweets_immi_daily, by=c("created_at" = "date", 'state' = 'state'))






Trump_tweet_daily <- Trump_tweet %>%
  mutate(negative = str_count(text, paste(keywords, collapse="|"))) %>%
  group_by(created_at) %>%
  summarise(freq = sum(negative > 0)/n(),
            total_negative = sum(negative > 0),
            total_tweet = n())
  
incident_num_daily <- 
  hate_crime %>% 
  select(DATA_YEAR, 
         INCIDENT_DATE, 
         STATE_ABBR, 
         BIAS_DESC, 
         OFFENDER_RACE) %>%
  filter(grepl('Anti-Hispanic or Latino', BIAS_DESC)) %>%
  filter( '2017-01-20' <= INCIDENT_DATE & INCIDENT_DATE <= '2017-04-20') %>%
  arrange(DATA_YEAR, INCIDENT_DATE) %>%
  group_by(INCIDENT_DATE, state = STATE_ABBR) %>%
  summarise(incident_number = n())


freq_trump_daily <- Trump_tweet_daily
freq_trump_daily$state <- "CA"
for (state in unique(incident_num_daily$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_daily
    tmp$state <- state
    freq_trump_daily <- rbind(freq_trump_daily, tmp)
  }
}


df_daily <- freq_trump_daily %>% 
  left_join(incident_num_daily, by=c("created_at" = "INCIDENT_DATE", 'state' = 'state')) %>%
  replace_na(list('incident_number' = 0))


View(df_daily)
fit <- plm(incident_number ~ freq , data=df_daily, 
           index = c("state", "created_at"),
           model = "within",
           effect = "individual") 

coeftest(fit, vcov=vcovHC(fit, type="sss", cluster="group")) 