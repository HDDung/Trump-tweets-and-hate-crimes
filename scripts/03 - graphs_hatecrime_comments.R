## input: 
    ## hate-speed.RData
    ## trump.RData
source("scripts/02 - aggregate.R")
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

twitter_usage <- read_csv("raw-data/twitter_usage_yearly.csv")
source("data_HateCrimeTrump.csv")


# For Introduction 

hate_crime %>%
  group_by(DATA_YEAR) %>%
  filter_at(vars(starts_with("DATA_YE")), all_vars(. > 2007)) %>%
  summarise(incident_num = n()) %>%
  ggplot(aes(x = DATA_YEAR, y = incident_num))+
  geom_line(size=1.5, col="grey31") +
  ggtitle("Hate Crimes per Year") +
  scale_x_continuous("Years", breaks = seq(2007, 2018, 1))+
  scale_y_continuous("Yearly number of Hate Crimes") +
  theme( plot.title = element_text(size= 18, vjust=2, hjust=0.5),
         axis.title.x = element_text(vjust=-1),
         axis.line = element_line(color = "grey31"),
         panel.background = element_blank()) +
  geom_vline(xintercept  = 2014, linetype="dotted", 
             color = "blue", size=2)


hate_crime %>% 
  filter_at(vars(starts_with("DATA_YE")), all_vars(. > 2007)) %>%
  mutate(BIAS_DESC = strsplit(as.character(BIAS_DESC), ";")) %>% 
  unnest(BIAS_DESC) %>%
  filter(BIAS_DESC==c("Anti-White", "Anti-Black or African American", 
                      "Anti-Asian", "Anti-American Indian or Alaska Native",
                      "Anti-Hispanic or Latino")) %>%
  group_by(DATA_YEAR, BIAS_DESC) %>%
  summarise(incident_num = n()) %>%
  ggplot(aes(x = DATA_YEAR, y = incident_num)) +
  geom_line(aes(col = BIAS_DESC), size=1) +
  ggtitle("Yearly Hate Crimes per Ethnic Group") +
  labs(col = "Racial Bias") +
  scale_x_continuous("Years", breaks = seq(2007, 2018, 1))+
  scale_y_continuous("Yearly number of Hate Crimes") +
  theme( plot.title = element_text(size= 18, vjust=2, hjust=0.5),
         axis.title.x = element_text(vjust=-1),
         axis.line = element_line(color = "grey31"),
         panel.background = element_blank()) +
  geom_vline(xintercept  = 2014, linetype="dotted", 
             color = "grey35", size=1.5)



# NOT USED

# ## For the USA (total_tweet, can be positive or negative)
# data_HateCrimeTrump %>%
#   group_by(week) %>%
#   summarise(total_tweet  = sum(total_tweet)/n(),
#             total_inc_num = sum(incident_number)) %>%
#   gather(key = "variable", value = "value", -week) %>%
#   ggplot(aes(x = week, y = value)) +
#   geom_line(aes(color = variable), size=1) + 
#   scale_color_manual(values = c("deep sky blue", "MediumOrchid4"))+ 
#   labs(title="Timeline of number of Trump tweets and Hate Crime (over all States)", col="Legend") +
#   scale_x_continuous("Weeks", breaks = seq(1, 13, 1))+
#   scale_y_continuous(" Total Number of Trump tweets / Number of Hate Crime") +
#   theme( plot.title = element_text(size= 18, vjust=2, hjust=0.5),
#          axis.title.x = element_text(vjust=-1),
#          axis.line = element_line(color = "grey31"),
#          panel.background = element_blank()) 
# 
# # We can see that the number of hate crime incidents (Mexican) and overall number of Trump Tweets coincide. 
# # The total number of Trump tweets shows how active Trump was on Twitter in each week. It takes into ccount all of his tweets,
# # either positive or negative. We can see, that in weeks Trump was more active on Twitter the Number of hate crimes also seems 
# # to be higher. This doesn't necessarily means that Trump has an influence on Hate Crime, there could also be a reverse causality.
# # Meaning that a higher number of Hate Crime leads Trump to post about it more. 


# For explanatory Analysis 

## Anti-Hispanic hate crime (whole US)
data_HateCrimeTrump %>%
  group_by(week) %>%
  summarise(NegTrumpTweets_pct_share = sum(freq)*100/n(),
            HateCrimeTotal = sum(incident_number)) %>%
  gather(key = "variable", value = "value", -week) %>%
  ggplot(aes(x = week, y = value)) +
  geom_line(aes(color = variable), size=1) + 
  scale_color_manual(values = c("deep sky blue", "MediumOrchid4")) +
  labs(title="Anti-Hispanic Trump tweets and anti-Hispanic hate crime (U.S.)") +
  labs(col = "Legend") +
  scale_x_continuous("Weeks", breaks = seq(1, 13, 1))+
  scale_y_continuous("Percentage Share of negative tweets / Total number of Hate Crimes") +
  theme( plot.title = element_text(size= 18, vjust=2, hjust=0.5),
         axis.title.x = element_text(vjust=-1),
         axis.line = element_line(color = "grey31"),
         panel.background = element_blank()) 


# With this graph we get a first glimpse at how Trump's Twitter activity and the number of hate crimes changed over time. 
# The y-axes describes two different variables with different scales. This might be a little problematic, but since it's just an explanatory graph
# only a minor issue. In the graph we can see that there is a coinciding tendency of the two trends, with hate crime incidents shifted a little to the right.
# This would mean that weeks, in which Trump used a lot of negative anti-Hispanic rhetoric in relation to his overall number of tweets, seem to be followed by weeks with more 
# hate crimes. To point out an explicit example we can see that in the second week after his inaugguration aroung 16 % out of all his tweets were directed against 
# Hispanics, wheras in the third week he only used anti-Hispanic rhetoric in around 2,5 % of his tweets. Respectively, the third week showes a peek on anti-Hispanic hate crime,
# wheras in the forth week tere is a steep drop in hate crimes. Trump tweets therefore seem to preceed the hate crime trend by a week. 
# Of course this is just a visual assesment. To get a better idea of the causal relationship we contucted a regression analysis down below. 


## Hate crime

twitter_usage_state <- twitter_usage %>%
  group_by(state) %>%
  summarise(mean_usage14_15 = sum(total)/n()) # take the mean hashtag counts for years 2014 and 2015

twitter_usage_state$mean_usage14_15 <- 
  scales::rescale(twitter_usage_state$mean_usage14_15, to=c(0,100)) # rescaling




install.packages("ggrepel")
library(ggrepel)


hate_crime %>%
  group_by(DATA_YEAR, STATE_ABBR) %>%
  summarise(incident_num = n()) %>%
  left_join(twitter_usage_state, by=c('STATE_ABBR' = 'state')) %>%
  filter(2015 <= DATA_YEAR & DATA_YEAR <= 2018) %>%
  ggplot(aes(x=mean_usage14_15, y=incident_num)) +
  geom_point() +
  geom_text(aes(label= STATE_ABBR), vjust = 0, nudge_y = 10, check_overlap = T) +
  labs(title="Twitter Use and Hate Crime (per State)", x="Twitter Usage (Rescaled to range 0 - 100)", y="Anti-Hispanic Hate Crimes") +
  labs(col = "State")+
  theme( plot.title = element_text( hjust=0.5),
         axis.title.x = element_text(vjust=-1),
         axis.line = element_line(color = "grey31"),
         panel.background = element_blank()) +
  facet_grid(. ~ DATA_YEAR)


# 
# hate_crime %>%
#   group_by(DATA_YEAR, STATE_ABBR) %>%
#   summarise(incident_num = n()) %>%
#   left_join(twitter_usage_state, by=c('STATE_ABBR' = 'state')) %>%
#   filter(2015 <= DATA_YEAR & DATA_YEAR <= 2018) %>%
#   ggplot(aes(x=mean_usage14_15, y=incident_num, col=STATE_ABBR)) +
#   geom_point() +
#   labs(title="Twitter Use and Hate Crime (per State)", x="Twitter Usage (Rescaled to range 0 - 100)", y="Anti-Hispanic Hate Crimes") +
#   labs(col = "State")+
#   theme( plot.title = element_text( hjust=0.5),
#          axis.title.x = element_text(vjust=-1),
#          axis.line = element_line(color = "grey31"),
#          panel.background = element_blank()) +
#    facet_grid(. ~ DATA_YEAR)

# The next graph looks at the Twitter Usage and the number of Anti-Hispanic hate crime per State. 
# In this plot we can see, that States with a large Twitter use also seem to have a higher number in hate crimes. 
# There therefore seems to be a correlation between Twitter in general and Anti-Hispanic hate crime. To assess if 
# this correlation could be induced by Trump tweets, we will have a closer look on Trumps Twitter activity and Hate Crime in the next graph.
# The States that seem to have a high Twitter Usage and a high crime hate are States with a larger population size in general. 
# The graphical relation might therefore also just be a consequence of this higher population size.
