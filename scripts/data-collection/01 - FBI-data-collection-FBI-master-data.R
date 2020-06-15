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
  filter(grepl('Anti-Hispanic', BIAS_DESC)) %>%
  filter( '2017-01-20' <= INCIDENT_DATE & INCIDENT_DATE <= '2017-04-20') %>%
  add_row(INCIDENT_DATE = as.Date("2017-01-20")) %>%
  arrange(INCIDENT_DATE) %>%
  mutate(week = cut.Date(INCIDENT_DATE, breaks = "1 week", labels = FALSE)) %>%
  filter(!is.na(BIAS_DESC)) %>%
  arrange(DATA_YEAR, INCIDENT_DATE, week) %>%
  group_by(week, state = STATE_ABBR) %>%
  summarise(incident_number = n())

