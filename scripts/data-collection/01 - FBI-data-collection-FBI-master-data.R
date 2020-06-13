library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)

hate_crime <- read_csv("raw-data/hate_crime.csv")
hate_crime$INCIDENT_DATE <- dmy(hate_crime$INCIDENT_DATE)
hate_crime$BIAS_DESC <- factor(hate_crime$BIAS_DESC)



incident_num_weekly <- 
  hate_crime %>% 
  select(DATA_YEAR, 
         INCIDENT_DATE, 
         STATE_ABBR, 
         BIAS_DESC, 
         OFFENDER_RACE) %>%
  filter(grepl('Anti-Hispanic or Latino', BIAS_DESC)) %>%
  filter( '2017-01-20' <= INCIDENT_DATE & INCIDENT_DATE <= '2017-04-20') %>%
  mutate(week = cut.Date(INCIDENT_DATE, breaks = "1 week", labels = FALSE)) %>%
  arrange(DATA_YEAR, INCIDENT_DATE, week) %>%
  group_by(week, state = STATE_ABBR) %>%
  summarise(incident_number = n())

# ggplot(incident_num_weekly[incident_num_weekly$state == 'SD',], aes(x = week, y = total)) +
#   geom_line(group = 1)
# 
# incident_num_yearly <- 
#   hate_crime %>% 
#   select(DATA_YEAR, 
#          STATE_ABBR, 
#          BIAS_DESC, 
#          OFFENDER_RACE) %>%
#   filter(grepl('Anti-Hispanic or Latino', BIAS_DESC)) %>%
#   arrange(DATA_YEAR) %>%
#   group_by(year = DATA_YEAR, state = STATE_ABBR) %>%
#   summarise(total = n())
# 
# incident_num_yearly
# 
# 
# ggplot(incident_num_yearly[incident_num_yearly$state == 'SD',], aes(x = year, y = total, col=state)) +
#   geom_bar(stat = "identity")
# 
# 
# View(hate_crime %>% 
#   select(DATA_YEAR, 
#          BIAS_DESC,
#          AGENCY_TYPE_NAME) %>%
#     filter(grepl('County', AGENCY_TYPE_NAME)) %>%
#     arrange(DATA_YEAR) %>%
#     group_by(year = DATA_YEAR) %>%
#     summarise(total = n()))
# 
# tmp <- hate_crime %>%
#   unite(ID, INCIDENT_ID,ORI, remove = FALSE, sep = "_")
# length(unique(tmp[tmp$DATA_YEAR == 2018,]$ID))
