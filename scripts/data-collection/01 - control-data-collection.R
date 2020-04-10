library(tidyverse)
library(jsonlite)

## Inital control list
control <- list()

for (year in c(2014:2018)) {
  control_list <- list()
  base_url <-
    paste(
      "https://api.census.gov/data/",
      year,
      "/acs/acs1?key=da1d3ad69181e635bf44bd677a9324b2039ceb80",
      sep = ""
    )
  
  #######################################################################
  ##### scrape data population ######
  #######################################################################
  
  ##
  pop_1year <-
    fromJSON(paste(base_url, "&get=B01003_001E&for=state:*", sep = ""))
  control_list[[paste("population", year, sep = "_")]] <-
    data.frame(
      year = year,
      population = as.numeric(pop_1year[-1, 1]),
      state = pop_1year[-1, 2]
    )
  
  remove(pop_1year)
  
  #######################################################################
  ##### scrape umemployment ######
  #######################################################################
  
  ##
  unem_1year <-
    fromJSON(paste(base_url, "&get=B18120_012E&for=state:*", sep = ""))
  
  unem_1year <-
    data.frame(
      year = year,
      umemployment = as.numeric(unem_1year[-1, 1]),
      state = unem_1year[-1, 2]
    )
  
  labor_1year <-
    fromJSON(paste(base_url, "&get=B18120_002E&for=state:*", sep = ""))
  
  labor_1year <-
    data.frame(
      year = year,
      labor_force = as.numeric(labor_1year[-1, 1]),
      state = labor_1year[-1, 2]
    )
  
  control_list[[paste("unempl_rate", year, sep = "_")]] <-
    unem_1year %>%
    inner_join(labor_1year) %>%
    mutate(rate = round(umemployment / labor_force, 2)) %>%
    select(state, year, rate)
  
  remove(unem_1year, labor_1year)
  
  #######################################################################
  ##### scrape MEDIAN HOUSEHOLD INCOME ######
  #######################################################################
  
  ## median household income the past 12 months (in 2018 inflation-adjusted dollars) by tenure
  median_income_1year <-
    fromJSON(paste(base_url, "&get=B25119_001E&for=state:*", sep = ""))
  
  control_list[[paste("income", year, sep = "_")]] <-
    data.frame(
      year = year,
      income = as.numeric(median_income_1year[-1, 1]),
      state = median_income_1year[-1, 2]
    )
  remove(median_income_1year)
  
  
  control[[as.character(year)]] <- Reduce(function(x, y)
    full_join(x, y),
    control_list)
  
  remove(control_list, base_url)
}

## Combine all controlling tables
control <- bind_rows(control)
