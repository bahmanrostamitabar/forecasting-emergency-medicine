library(hts)
library(readr)
library(furrr)
source(here::here("rscript/hts/htsplus.R"))
source(here::here("rscript/hts/tscount.R"))

# Parallelization
plan(multisession, workers = 2)

# Read hierarchical/grouped time series
#incident_gts <- read_rds(here::here("data/incidents_test_gts.rds"))
incident_gts <- read_rds(here::here("data/incidents_gts.rds"))
holidays <- read_rds(here::here("data/holidays_ts.rds"))

train <- window(incident_gts, end = c(190, 7))
test <- window(incident_gts, start = c(191, 1))
alltest <- aggts(test)

# ETS simulations
fcst_ets <- htsplus(train, h = nrow(test$bts),
    model_function = ets, methods = c("bu", "wls", "mint")
  )
# TSCOUNT simulations
fcst_tscount <- htsplus(train, h = nrow(test$bts),
    model_function = tscount, method = c("bu", "wls", "mint")
  )
  
rmsse_ets <- rmsse(fcst_ets, test, train)
crps_ets <- crps(fcst_ets, test)
rmsse_tscount <- rmsse(fcst_tscount, test, train)
crps_tscount <- crps(fcst_tscount, test)
