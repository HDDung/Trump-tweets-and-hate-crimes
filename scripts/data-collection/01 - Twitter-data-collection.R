library(rtweet)
library(twitteR)
library(readr)

## setup twitter authentication
# setup_twitter_oauth(
#   consumer_key    = "",
#   consumer_secret  = "",
#   access_token     = "",
#   access_secret    = ""
# )
# create_token(
#   app = "fcdd-course",
#   consumer_key = "",
#   consumer_secret = "",
#   access_token = "",
#   access_secret = ""
# )


## Load Trump's tweets
# trump_tweets <-
#   read_csv("raw-data/trump_tweets.csv",
#            col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M"),
#                             id = col_character()))


## Loading tweets related to Trump's speech

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




