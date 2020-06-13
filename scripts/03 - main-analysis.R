## input: 
    ## hate-speed.RData
    ## trump.RData
source("scripts/02 - aggregate.R")
library(readr)

twitter_usage <- read_csv("raw-data/twitter_usage_yearly.csv")



hate_crime %>%
  group_by(DATA_YEAR) %>%
  summarise(incident_num = n()) %>%
  ggplot(aes(x = DATA_YEAR, y = incident_num)) +
  geom_line() +
  geom_vline(xintercept  = 2014, linetype="dotted", 
             color = "red", size=1.5)

hate_crime %>% 
  mutate(BIAS_DESC = strsplit(as.character(BIAS_DESC), ";")) %>% 
  unnest(BIAS_DESC) %>%
  group_by(DATA_YEAR, BIAS_DESC) %>%
  summarise(incident_num = n()) %>%
  ggplot(aes(x = DATA_YEAR, y = incident_num)) +
  geom_line(aes(col = BIAS_DESC)) +
  geom_vline(xintercept  = 2014, linetype="dotted", 
             color = "red", size=1.5)

## For CA
data_HateCrimeTrump %>%
  filter(state == 'CA') %>%
  select(week, freq, incident_number) %>%
  mutate(freq = freq * 100) %>%
  gather(key = "variable", value = "value", -week) %>%
  ggplot(aes(x = week, y = value)) +
    geom_line(aes(color = variable)) + 
    scale_color_manual(values = c("darkred", "steelblue"))

## For the USA (Freq)
data_HateCrimeTrump %>%
  group_by(week) %>%
  summarise(freq = sum(freq)*100/n(),
            total_inc_num = sum(incident_number)) %>%
  gather(key = "variable", value = "value", -week) %>%
  ggplot(aes(x = week, y = value)) +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) + 
  labs(title="For the USA (Frequency of negative tweets per week)")


## For the USA (total_negative)
data_HateCrimeTrump %>%
  group_by(week) %>%
  summarise(total_negative  = sum(total_negative)/n(),
            total_inc_num = sum(incident_number)) %>%
  gather(key = "variable", value = "value", -week) %>%
  ggplot(aes(x = week, y = value)) +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("steelblue", "darkred"))+ 
  labs(title="For the USA (Total Trump negative tweets per week)")


## For the USA (total_tweet)
data_HateCrimeTrump %>%
  group_by(week) %>%
  summarise(total_tweet  = sum(total_tweet)/n(),
            total_inc_num = sum(incident_number)) %>%
  gather(key = "variable", value = "value", -week) %>%
  ggplot(aes(x = week, y = value)) +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("steelblue", "darkred"))+ 
  labs(title="For the USA (total Trump tweets per week)")

## For the USA (total_tweet)
data_HateCrimeTrump %>%
  group_by(week) %>%
  summarise(total_tweet  = sum(total_tweet)/n(),
            total_inc_num = sum(incident_number)) %>%
  gather(key = "variable", value = "value", -week) %>%
  ggplot(aes(x = week, y = value)) +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("steelblue", "darkred"))+ 
  labs(title="For the USA (total Trump tweets per week)")

## Hate crime
twitter_usage_state <- twitter_usage %>%
  group_by(state) %>%
  summarise(usage = sum(total)/n())

twitter_usage_state$usage <- 
  scales::rescale(twitter_usage_state$usage, to=c(0,10))

hate_crime %>%
  group_by(DATA_YEAR, STATE_ABBR) %>%
  summarise(incident_num = n()) %>%
  mutate(incident_num = scales::rescale(incident_num, to=c(0,10))) %>%
  left_join(twitter_usage_state, by=c('STATE_ABBR' = 'state')) %>%
  filter(2015 <= DATA_YEAR & DATA_YEAR <= 2018) %>%
  gather(key = "variable", value = "value", -DATA_YEAR, -STATE_ABBR) %>%
  ggplot(aes(x = STATE_ABBR, y = value, col, col=variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ DATA_YEAR)


hate_crime %>%
  group_by(DATA_YEAR, STATE_ABBR) %>%
  summarise(incident_num = n()) %>%
  mutate(incident_num = scales::rescale(incident_num, to=c(0,10))) %>%
  left_join(twitter_usage_state, by=c('STATE_ABBR' = 'state')) %>%
  filter(2018 == DATA_YEAR) %>%
  gather(key = "variable", value = "value", -DATA_YEAR, -STATE_ABBR) %>%
  ggplot(aes(x = STATE_ABBR, y = value, col, col=variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.2), alpha = 0.6) + 
  facet_grid(. ~ DATA_YEAR)

library(lmtest)
library(multiwayvcov)

data_fe

fit <- plm(incident_number ~ freq , data=data_HateCrimeTrump, 
    index = c("state_name", "week"),
    model = "within",
    effect = "individual") 

coeftest(fit, vcov=vcovHC(fit, type="sss", cluster="group")) 


stargazer( fit, 
           type = "text", no.space = TRUE, single.row = TRUE, header = FALSE)

library(Ecdat)
library(plm)
data(Grunfeld)
pdata.frame(Grunfeld,"firm","year")
gi <- plm(inv ~ value + capital, data=Grunfeld)
FE(gi)
FE(gi$within)
summary(FE(gi))  
  
  
  
  