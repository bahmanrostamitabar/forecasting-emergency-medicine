library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(fable.tscount)

# Read data previously processed in data folder and add aggregation levels
incidents <- readRDS(here::here("data/incidents_tsbl.rds")) |>
  aggregate_key(nature_of_incident * category * lhb_code,
    incidents = sum(incidents)
  )

# Add holidays
incidents <- incidents |>
  left_join(
    readRDS(here::here("data/holiday_dummy.rds")),
    by = "date"
  )

# Check that some of the upper levels look ok
incidents |>
  filter(is_aggregated(nature_of_incident), is_aggregated(category)) |>
  autoplot(incidents)
incidents |>
  filter(is_aggregated(nature_of_incident), is_aggregated(lhb_code)) |>
  autoplot(incidents)

# First pass using a simple training/test set keeping last 6 weeks for testing
train <- filter(incidents, date <= max(date) - 42)
test <- filter(incidents, date > max(date) - 42)

# Fit some models
fit_incident <- train %>%
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS = ETS(sqrt(incidents)),
    TSCOUNT = TSCOUNT(incidents ~ trend() + season("week") + fourier("year", 10)
        + public_holiday_d + school_holiday_d + xmas + new_years_day, 
      link = "log", model = list(past_obs = 1:13))
  )
# Add reconciliation constraints and methods
fit_reconcile <- fit_incident %>%
  reconcile(
    td_TSCOUNT = top_down(TSCOUNT),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    wls_TSCOUNT = min_trace(TSCOUNT, method = "wls_struct"),
    td_ETS = top_down(ETS),
    bu_ETS = bottom_up(ETS),
    wls_ETS = min_trace(ETS, method = "wls_struct")
  )

# Produce forecasts
fcst_incident <- fit_reconcile %>%
  forecast(new_data = test)

# Look at MASE over several levels for MinT
fcst_accuracy <- fcst_incident %>%
  accuracy(incidents_gts, measures = list(rmsse = RMSSE, mase = MASE))

# Bottom level
fcst_accuracy %>%
  filter(!is_aggregated(lhb_code), !is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))

# At LHB level
fcst_accuracy %>%
  filter(is_aggregated(lhb_code), !is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))
# At category level
fcst_accuracy %>%
  filter(!is_aggregated(lhb_code), is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))
# Top level
fcst_accuracy %>%
  filter(is_aggregated(lhb_code), is_aggregated(category), is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))


# Now repeat the process using time series cross validation
n_init <- length(incidents_gts_holidays %>% filter_index(. ~ "2018-07-30") %>% pull(date) %>% unique())
train_tscv <- incidents_gts_holidays %>%
  filter_index(. ~ "2019-06-27") %>%
  stretch_tsibble(.init = n_init, .step = 6)

# this is to create the test set and assign it to new_data in forecast
test_tscv <- incidents_gts_holidays %>%
  filter_index("2018-07-31" ~ .) %>%
  slide_tsibble(.size = f_horizon, .step = 6)

# Specify estimating models using multiple core
library(future)
plan(multicore)
fit_incident <- train_tscv %>%
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS = ETS(sqrt(incidents)),
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend() + season("week") + fourier("year", 10) + public_holiday_d + school_holiday_d + xmas + new_years_day, link = "log", model = list(past_obs = 1:13))
  )

fit_reconcile <- fit_incident %>%
  reconcile(
    td_TSCOUNT = top_down(TSCOUNT),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    wls_TSCOUNT = min_trace(TSCOUNT, method = "wls_struct"),
    td_ETS = top_down(ETS),
    bu_ETS = bottom_up(ETS),
    wls_ETS = min_trace(ETS, method = "wls_struct")
  )

fcst_incident <- fit_reconcile %>%
  forecast(new_data = test_tscv)

# Look at MASE over several levels for MinT
fcst_accuracy <- fcst_incident %>%
  accuracy(incidents_gts,
    measures = list(rmsse = RMSSE, mase = MASE)
  )

# Bottom level
fcst_accuracy %>%
  filter(!is_aggregated(lhb_code), !is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))

# At LHB level
fcst_accuracy %>%
  filter(is_aggregated(lhb_code), !is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))
# At category level
fcst_accuracy %>%
  filter(!is_aggregated(lhb_code), is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))
# Top level
fcst_accuracy %>%
  filter(is_aggregated(lhb_code), is_aggregated(category), is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps = mean(crps), winkler = mean(winkler))
