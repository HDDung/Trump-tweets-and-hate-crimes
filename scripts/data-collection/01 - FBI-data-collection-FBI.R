library(xml2)
library(rvest)
library(robotstxt)
library(dplyr)
library(parallel)

#######################################################################
##### predefined function  ######
#######################################################################

## function get table from given url
get_table <- function(url, state, year) {
  table <- tryCatch({
    src <- read_html(url)
    html_table(src, fill = TRUE)
  }, error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  }, warning = function(cond) {
    message(paste("URL caused a warning:", url))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  })
  if (!is.na(table)) {
    ## Only choose value from agency
    table <-
      table[[1]][(table[[1]]$`Agency name` != "" &
                    # only take columns 2, 1, 3: 2 - agency name, 1 - type of agency, 3 - incidents
                    table[[1]]$`Agency name` != "State Police:"), c(2, 1, 3)]
    table <- table[-1, ]
    table$state <- state
    table$year <- year
  } else {
    table <- data.frame()
    table$`Agency name` <- NA
    table$`Agency type` <- NA
    table$`Number of incidents per bias motivation` <- NA
    table$state <- state
    table$year <- year
  }
  return(table)
}

## Checking legality of scraping
paths_allowed("https://ucr.fbi.gov/hate-crime/2017/tables/table-13.xls/view")


#######################################################################
##### Scrap hate crime total  ######
#######################################################################

#Initial data frame for url
df_url <- data.frame(year = integer(),
                     state = character(),
                     url = character())

## setup urls of the 2017
url <-
  read_html("https://ucr.fbi.gov/hate-crime/2017/tables/table-13.xls/view")
nds <- html_nodes(url,
                  xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "arrow-left-large", " " ))]')
states <- html_text(nds)
states <- tolower(states)
states <- gsub(" ", "-", states)
array_url <- sapply(states, function(state) {
  paste(
    "https://ucr.fbi.gov/hate-crime/2017/tables/table-13-state-cuts/",
    state,
    ".xls",
    sep = ""
  )
})
tmp <- data.frame(year = 2017, state = states, url = array_url)
rownames(tmp) <- NULL
df_url <- rbind(df_url, tmp)


## setup urls of the 2016
### get list state
url <-
  read_html(
    "https://ucr.fbi.gov/hate-crime/2016/tables/table-13/table_13_hate_crime_incidents_per_bias_motivation_and_quarter_by_state_and_agency_2016.xls/view"
  )
nds <- html_nodes(url,
                  xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "arrow-left-large", " " ))]')
states <- html_text(nds)

### standardize the format
states <- tolower(states)
states <- gsub(" ", "_", states)

### create urls
array_url <- c(array_url, sapply(states, function(state) {
  paste(
    "https://ucr.fbi.gov/hate-crime/2016/tables/table-13/table-13-state-cuts/table_13_",
    state,
    "_hate_crime_incidents_per_bias_motivation_and_quarter_by_agency_2016.xls",
    sep = ""
  )
}))
tmp <- data.frame(year = 2016, state = states, url = array_url)
rownames(tmp) <- NULL
df_url <- rbind(df_url, tmp)


### initial data frame
hate_crime_total <- data.frame()

##scraping data
apply(df_url, 1, function(row, output) {
  table <-
    get_table(url = row['url'],
              state = row['state'],
              year = row['year'])
  names(table) <- tolower(names(table))
  assign("hate_crime_total",
         rbind(hate_crime_total, table),
         envir = parent.frame(n = 2))
  remove(table)
})

## move year to the first
hate_crime_total <- hate_crime_total[, c(5, 4, 1, 2, 3)]

## normalize column name and their values
names(hate_crime_total) <- gsub(" ", "_", names(hate_crime_total))

hate_crime_total$state <-
  gsub("-", "_", hate_crime_total$state)

hate_crime_total$agency_name <-
  tolower(hate_crime_total$agency_name)
hate_crime_total$agency_name <-
  gsub(" ", "_", hate_crime_total$agency_name)


## write down csv
write.csv(hate_crime_total, file = "processed-data/hate_crime_total.csv", row.names = FALSE)


#######################################################################
##### Scrap hate crime per ethnic  ######
#######################################################################

## List of urls
array_url <- c(
  "https://ucr.fbi.gov/hate-crime/2018/topic-pages/tables/table-1.xls",
  "https://ucr.fbi.gov/hate-crime/2017/topic-pages/tables/table-1.xls",
  "https://ucr.fbi.gov/hate-crime/2016/tables/table-1",
  "https://ucr.fbi.gov/hate-crime/2015/tables-and-data-declarations/1tabledatadecpdf",
  "https://ucr.fbi.gov/hate-crime/2014/tables/table-1",
  "https://ucr.fbi.gov/hate-crime/2013/tables/1tabledatadecpdf/table_1_incidents_offenses_victims_and_known_offenders_by_bias_motivation_2013.xls"
)


### initial data frame
hate_crime_per_ethnic <- data.frame()

## Scrap hate crime
i <- 1
for (url in array_url) {
  src <- read_html(url)
  table <- html_table(src, fill = TRUE)
  if (2018 - (i - 1) == 2013) {
    table <- table[[2]]
  } else {
    table <- table[[1]]
  }
  table$year <- 2018 - (i - 1)
  i <- i + 1
  hate_crime_per_ethnic <- rbind(hate_crime_per_ethnic, table)
}

## move year to the first
hate_crime_per_ethnic <- hate_crime_per_ethnic[, c(6, 1:5)]

## normalize column names and their values
names(hate_crime_per_ethnic) <-
  tolower(names(hate_crime_per_ethnic))
names(hate_crime_per_ethnic) <-
  gsub(" ", "_", names(hate_crime_per_ethnic))


## write down csv
write.csv(hate_crime_per_ethnic, file = "processed-data/hate_crime_per_ethnic.csv", row.names = FALSE)
