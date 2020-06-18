# BAIT data and code for shiny app

## BAIT data
All data can be found in the data directory of this repository. Data is saved as binary rds files that can be opened in R with `readr::read_rds()` or `base::readRDS()`.

## Deploy Shiny app on shinyapps.io.

```
library(rsconnect)
#Only need to run setAccountInfo the first time deploying from a computer.  
rsconnect::setAccountInfo(name='...', token='...', secret> ...')
# set BioC repositories so shinyapps.io can install bioc packages
# options(repos = BiocManager::repositories())
# getOption("repos")
# output of this command should show repositories BioCsoft, BioCann, BioCexp, BioCworkflows, and CRAN
# Make sure you're in the directory containing app.R
deployApp(appFiles = c("app.R", "data", "R"))
```


