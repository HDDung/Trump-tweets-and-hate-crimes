library(rtweet)
library(readr)
library(twitteR)

# setup twitter authentication

create_token(
  app = "trump-details",
  consumer_key    = "1f1kYYGptyKnwauuMR5Fo6QTX",
  consumer_secret  = "Rkf7NJvt7B1BzEPy8zCd79qI4HUhRrzUP63mD8zHUDWq70uwzC",
  access_token     = "1243602264843042820-s3rFUNlgBTrf33cbQDRwM3TpMROQnI",
  access_secret    = "QtOQmvBHiyVWTYTVHnzkS5vZhcVfoQzEWHR3h37a1b4hB"
)




trump_flw_first_round <- get_followers("realDonaldTrump", n = 75000, retryonratelimit = TRUE)
trump_flw_first_round
View(trump_flw_first_round)
trump_flw_data <- lookup_users(trump_flw_first_round$user_id)

write.csv(trump_flw_first_round,
            file = "processed-data/Trump_follower_IDs.csv")
trump_flw <- get_followers("realDonaldTrump", n = 820000, retryonratelimit = TRUE)

View(trump_flw_data)
write.csv(trump_flw_data$screen_name,
          file="processed-data/Trump_follower_ScreenName.csv", row.names = FALSE)

#state_codes <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
state_codes <- c("A", "B")

related_tweets <- data.frame()
for (state in state_codes) {
  file_path <-
    paste("raw-data/related_tweets_", state, ".csv", sep = "")
  
  df <-
    read_csv(file_path,
             col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M"),
                              id = col_character()))
  df$state <- state
  df[,c(length(df),1:(length(df)-1))]
  related_tweets <- rbind(related_tweets, df)
  remove(df, file_path, state)
}
## Change state column to the first
related_tweets <- 
  related_tweets[,c(length(related_tweets),1:(length(related_tweets)-1))]

## Combining 




## Output data


write.csv(related_tweets, file="processed-data/related_tweets.csv", row.names=FALSE)
remove(related_tweets, state_codes)




