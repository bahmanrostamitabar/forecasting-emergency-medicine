library(hts)
source(here::here("rscript/hts/htsplus.R"))

# Read hierarchical/grouped time series
incident_gts <- read_rds(here::here("data/incidents_test_gts.rds"))
#incident_gts <- read_rds(here::here("data/incidents_gts.rds"))

train <- window(incident_gts, end = c(12,1))
test <- window(incident_gts, start=c(12,2))

# Simulated future sample paths
fcst_ets_bu <- htsplus(train, h = nrow(test$bts), model_function = ets, lambda = 0.5, method = "bu")
fcst_ets_wls <- htsplus(train, h = nrow(test$bts), model_function = ets, lambda = 0.5, method = "wls")
fcst_ets_mint <- htsplus(train, h = nrow(test$bts), model_function = ets, lambda = 0.5, method = "mint")

# Means
mean_ets_bu <- t(apply(fcst_ets_bu, c(1,2), mean))
mean_ets_wls <- t(apply(fcst_ets_wls, c(1,2), mean))
mean_ets_mint <- t(apply(fcst_ets_mint, c(1,2), mean))

rmsse(mean_ets_bu, test, train)
rmsse(mean_ets_wls, test, train)
rmsse(mean_ets_mint, test, train)


