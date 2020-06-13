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

source("scripts/data-collection/01 - FBI-data-collection-FBI-master-data.R")
load("US Census raw data & File/sociodemographics_data.Rdata")

library(xml2)
library(rvest)
library(tidyr)

freq_trump <- freq_neg_tweet
freq_trump$state <- "CA"
for (state in unique(incident_num_weekly$state)){
  if (state != "CA"){
    tmp <- freq_neg_tweet
    tmp$state <- state
    freq_trump <- rbind(freq_trump, tmp)
  }
}

src_area <- read_html("https://www.infoplease.com/us/postal-information/state-abbreviations-and-state-postal-codes")
# store all tables in object "all_tables"

abb_state <- html_table(src_area, fill = TRUE)[[1]]
abb_state$state_name <- abb_state$`State/District` 


data_HateCrimeTrump <- 
  freq_trump %>%
  left_join(incident_num_weekly) %>%
  replace_na(list(incident_number = 0)) %>% 
  left_join(abb_state, by=c("state" = "Postal Code")) %>%
  select(week, state, state_name, freq, total_negative, total_tweet, incident_number) %>%
  left_join(sociodemographics, by=c('state_name' = 'state')) %>%
  arrange(week)

View(data_HateCrimeTrump)

write.csv(data_HateCrimeTrump, file = "processed-data/data_HateCrimeTrump.csv", row.names = FALSE)
