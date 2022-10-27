library(hts)
library(readr)
library(furrr)
source(here::here("rscript/hts/htsplus.R"))
source(here::here("rscript/hts/tscount.R"))

# Parallelization
#plan(multisession, workers = 3)

# Read hierarchical/grouped time series
#incident_gts <- read_rds(here::here("data/incidents_test_gts.rds"))
incident_gts <- read_rds(here::here("data/incidents_gts.rds"))
holidays <- read_rds(here::here("data/holidays_ts.rds"))

# Keep last 12 weeks for evaluation
train <- window(incident_gts, end = c(188, 7))
test <- window(incident_gts, start = c(188, 8))

# Create reconciled sample paths
reconcile_sample_paths(train, model_function = "ets")
reconcile_sample_paths(train, model_function = "tscount")

# Summary statistics of forecast accuracy
#rmsse_ets <- rmsse(train, test, model_function = "ets", method = "wls")
#crps_ets <- crps(fcst_ets, test)
#rmsse_tscount <- rmsse(fcst_tscount, test, train)
#crps_tscount <- crps(fcst_tscount, test)
