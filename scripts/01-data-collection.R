### Step 1: import data from twitter csv
source("./scripts/data-collection-scripts/01 - Twitter-data-collection-FBI.R")
## input: 
## output: 
    ## trump-tweets.RData
    ## related-tweets.RData


### Step 2: scraping data from FBI
source("./scripts/data-collection-scripts/01 - FBI-data-collection.R")
## input: 
## output: 
    ## ethnic-hate-crime.RData
    ## total-hate-crime.RData

### Step 3: scraping data for controlling variables
source("./scripts/data-collection-scripts/01 - control-data-collection.R")
## input: 
## output: data-control.RData