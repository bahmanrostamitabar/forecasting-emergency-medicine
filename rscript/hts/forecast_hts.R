library(hts)
library(readr)
library(furrr)
source(here::here("rscript/hts/htsplus.R"))
source(here::here("rscript/hts/tscount.R"))

# Parallelization
plan(multisession, workers = 2)

# Read hierarchical/grouped time series
incident_gts <- read_rds(here::here("data/incidents_test_gts.rds"))
# incident_gts <- read_rds(here::here("data/incidents_gts.rds"))
holidays <- read_rds(here::here("data/holidays_ts.rds"))

train <- window(incident_gts, end = c(190, 7))
test <- window(incident_gts, start = c(191, 1))
alltest <- aggts(test)

# Reconciliation methods
methods <- c("bu", "wls", "mint")

# Set up storage
fcst_ets <- as.list(methods)
names(fcst_ets) <- methods
fcst_tscount <- fcst_ets
rmsse_ets <- matrix(0, nrow = length(methods), ncol = ncol(alltest))
dimnames(rmsse_ets) <- list(methods, colnames(alltest))
rmsse_tscount <- crps_tscount <- crps_ets <- rmsse_ets

# ETS simulations
for (i in seq_along(methods)) {
  cat(paste("ETS: Method", methods[[i]], "\n"))
  fcst_ets[[i]] <- htsplus(train,
    h = nrow(test$bts),
    model_function = ets, method = methods[i]
  )
  rmsse_ets[i, ] <- rmsse(fcst_ets[[i]], test, train)
  crps_ets[i, ] <- crps(fcst_ets[[i]], test)
}
# TSCOUNT simulations
for (i in seq_along(methods)) {
  cat(paste("TSCOUNT: Method", methods[[i]], "\n"))
  fcst_tscount[[i]] <- htsplus(train,
    h = nrow(test$bts),
    model_function = tscount, method = methods[i]
  )
  rmsse_tscount[i, ] <- rmsse(fcst_tscount[[i]], test, train)
  crps_tscount[i, ] <- crps(fcst_tscount[[i]], test)
}
