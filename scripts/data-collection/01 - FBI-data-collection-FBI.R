library(xml2)
library(rvest)
library(robotstxt)
library(parallel)

paths_allowed("https://ucr.fbi.gov/hate-crime/2017/tables/table-13.xls/view")

url <- read_html("https://ucr.fbi.gov/hate-crime/2017/tables/table-13.xls/view")

nds <- html_nodes(
  url, 
  xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "arrow-left-large", " " ))]')

states <- html_text(nds)



out <- list()
for(i in 1:length(states)) {
  url <- paste("https://ucr.fbi.gov/hate-crime/2017/tables/table-13-state-cuts/",tolower(states[i]), ".xls", sep = "")
  src <- read_html(url)
  table <- html_table(src, fill = TRUE)
  out[states] <- table 
} 

View(out[[1]])

