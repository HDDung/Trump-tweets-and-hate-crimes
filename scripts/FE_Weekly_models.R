## Fixed effects regressions 

######################################################################################################


################# Aggregate daily data to weekly ######################


Trump_tweet_weekly <- freq_neg_tweet_hispanic %>%
  mutate(freq_weekly = total_negative/total_tweet) %>%
  view()


usa_tweets_immigration_wk <- 
  usa_tweets_hispanic %>%
  add_row(date = as.Date("2017-01-20"))  %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  arrange(date, week) %>%
  filter(!is.na(WordCount)) %>%
  group_by(week, state)
  ##  summarise(sentiment = mean(SentimentGI, na.rm = TRUE)) %>%

#################### WEEKLY: HATE CRIME ###########################################  





freq_trump_weekly_immi <- Trump_tweet_weekly
freq_trump_weekly_immi$state <- "CA"
for (state in unique(incident_num_weekly$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_weekly
    tmp$state <- state
    freq_trump_weekly_immi <- rbind(freq_trump_weekly_immi, tmp)
  }
}


df_crime_weekly_immi <- freq_trump_weekly_immi %>% 
  left_join(incident_num_weekly, by=c("week" = "week", 'state' = 'state')) %>%
  replace_na(list('incident_number' = 0)) %>%
  view()


fit_crime_immi <- plm(incident_number ~ freq_weekly*week , data=df_crime_weekly_immi, 
                      index = c("state", "week"),
                      model = "within",
                      effect = "individual")


coeftest(fit_crime_immi, vcov=vcovHC(fit_crime_immi, type="sss", cluster="group")) 



############################# Coronavirus level of sentiment model #############################################

usa_tweets_covid_wk <- usa_tweets_coronavirus %>%
  add_row(date = as.Date("2020-03-16"))  %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  arrange(date, week) %>%
  filter(!is.na(WordCount)) %>%
  group_by(week, state) %>%
  ##  summarise(sentiment = mean(SentimentGI, na.rm = TRUE)) %>%
  
  
  
freq_sentiment_covid_wk <- 
  usa_tweets_covid_wk %>%
  select(week, 
         SentimentGI,
         state) %>%
  arrange(week) %>%
  group_by(week, state) %>%
  count() 


freq_trump_weekly_covid <- Trump_tweet_weekly
freq_trump_weekly_covid$state <- "WI"
for (state in unique(freq_sentiment_covid_wk$state)){
  if (state != "WI"){
    tmp <- Trump_tweet_weekly
    tmp$state <- state
    freq_trump_weekly_covid <- rbind(freq_trump_weekly_covid, tmp)
  }
}


df_sentiment_weekly_covid <- freq_trump_weekly_covid %>% 
  left_join(freq_sentiment_covid_wk, by=c("week" = "week", 'state' = 'state')) %>%
  replace_na(list('n' = 0)) 


fit_freq_covid <- plm(n ~ freq_weekly , data=df_sentiment_weekly_covid, 
                      index = c("state", "week"),
                      model = "within",
                      effect = "individual")


coeftest(fit_freq_covid, vcov=vcovHC(fit_freq_covid, type="sss", cluster="group")) 




############## WEEKLY:Effects of Trump's tweets freq on the number of users' tweets Immigration ########

freq_sen_immi_wk <- 
  usa_tweets_immigration_wk %>%
  select(week, 
         SentimentGI, 
         state) %>%
  arrange(week) %>%
  group_by(week, state) %>%
  count() 



freq_trump_weekly_immi <- Trump_tweet_weekly
freq_trump_weekly_immi$state <- "CA"
for (state in unique(freq_sen_immi_wk$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_weekly
    tmp$state <- state
    freq_trump_weekly_immi <- rbind(freq_trump_weekly_immi, tmp)
  }
}


df_sentiment_weekly_immi <- freq_trump_weekly_immi %>% 
  left_join(freq_sen_immi_wk, by=c("week" = "week", 'state' = 'state')) 


fit_freq_immi <- plm(n ~ freq_weekly , data=df_sentiment_weekly_immi, 
                     index = c("state", "week"),
                     model = "within",
                     effect = "individual")


coeftest(fit_freq_immi, vcov=vcovHC(fit_freq_immi, type="sss", cluster="group")) 


######### WEEKLY: Effects of Trump's tweets freq on the sentiment of users' tweets Immigration ################


sentiment_tweets_immi_wk <- 
  usa_tweets_immigration %>%
  add_row(date = as.Date("2017-01-20"))  %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  arrange(date, week) %>%
  filter(!is.na(WordCount)) %>%
  group_by(week, state) %>%
  summarise(sentiment = mean(SentimentGI, na.rm = TRUE)) 



freq_trump_weekly_immi <- Trump_tweet_weekly
freq_trump_weekly_immi $state <- "CA"
for (state in unique(sentiment_tweets_immi_wk$state)){
  if (state != "CA"){
    tmp <- Trump_tweet_weekly
    tmp$state <- state
    freq_trump_weekly_immi <- rbind(freq_trump_weekly_immi, tmp)
  }
}



df_sentiment_wk_neg_immi <- freq_trump_weekly_immi %>% 
  left_join(sentiment_tweets_immi_wk, by=c("week" = "week", 'state' = 'state')) %>%
  view()



## Running the fixed effects regression
fit_sen_neg_wk_immi <- plm(sentiment ~ freq_weekly , data = df_sentiment_wk_neg_immi, 
                           index = c("state", "week"),
                           model = "within",
                           effect = "individual")


coeftest(fit_sen_neg_wk_immi, vcov=vcovHC(fit_sen_neg_wk_immi, type="sss", cluster="group"))



##################################################################################################################










