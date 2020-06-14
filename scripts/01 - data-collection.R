### Step 1: scraping data from FBI
source("scripts/data-collection/01 - FBI-data-collection-FBI-master-data.R.R")
## input: 
## output: 
    ## ethnic-hate-crime.RData
    ## total-hate-crime.RData

### Step 2: scraping data for controlling variables
source("scripts/data-collection/01 - control-data-collection.R")
## input: 
## output: data-control.RData