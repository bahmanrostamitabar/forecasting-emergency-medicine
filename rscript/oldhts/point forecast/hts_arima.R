library(tidyverse)
library(purrr)
library(future)
library(hts)
library(forecast)
library(tscount)
library(tsibble)
fcst_inciddent <- function(train, test) {
  incident_all_train <- train %>% select(-public_holiday_d, -school_holiday_d, -xmas, -new_years_day)
  incident_all_test <- test %>% select(-public_holiday_d, -school_holiday_d, -xmas, -new_years_day)
  holiday_dummy_matrix_train <- train %>%
    select(public_holiday_d, school_holiday_d, xmas, new_years_day) %>%
    as.matrix()
  holiday_dummy_matrix_test <- test %>%
    select(public_holiday_d, school_holiday_d, xmas, new_years_day) %>%
    as.matrix()

  incident_ts_train <- ts((incident_all_train), frequency = 7)
  # create hierarchical/grouped time series
  gts_incident_train <- hts::gts(incident_ts_train,
    characters = list(c(1, 2), 3, 9)
  )
  incident_ts_test <- ts(incident_all_test, frequency = 7)
  gts_incident_test <- hts::gts(incident_ts_test,
    characters = list(c(1, 2), 3, 9)
  )
  comd_arima <- forecast(gts_incident_train,
    h = fh,
    method = "comb",
    fmethod = "arima",
    parallel = TRUE,
    lambda = .5
  )
  bu_arima <- forecast(gts_incident_train,
    h = fh, method = "bu",
    fmethod = "arima",
    parallel = TRUE,
    lambda = .5
  )

  ally <- aggts(gts_incident_train)
  allf <- matrix(NA, nrow = fh, ncol = ncol(ally))

  # How to get error for the forecast horizon: 1) use aggts(comd_arima) - gts_incident_test
  # accuracy.gts(comd_arima, gts_incident_test)[c(1,2,6),]

  arima_comd_all <- (aggts(comd_arima))
  arima_bu_all <- (aggts(bu_arima))

  arima_c <- as_tibble(arima_comd_all) %>% bind_cols(approach = rep("arima_comb", fh))
  arima_bu <- as_tibble(arima_bu_all) %>% bind_cols(approach = rep("arima_bu", fh))


  fcst_all <- arima_c %>% bind_rows(arima_bu)

  test_set <- as_tibble(aggts(gts_incident_test))

  arima_comb_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(arima_comd_all)) %>% bind_cols(approach = rep("arima_comb", fh))
  arima_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(arima_bu_all)) %>% bind_cols(approach = rep("arima_bu", fh))
  error <- arima_comb_e %>% bind_rows(arima_bu_e)

  arima_comb_test <- test_set %>% bind_cols(approach = rep("arima_comb", fh))
  arima_bu_test <- test_set %>% bind_cols(approach = rep("arima_bu", fh))

  test_data_all <- arima_comb_test %>%
    bind_rows(arima_bu_test)

  return(list(
    error_all_level = error,
    forecast_all_level = fcst_all,
    testset_all_level = test_data_all
  ))
}

incident_all <- read_rds("incident_all.rds")
# holidays_p <- read_rds("data/holiday_prophet.rds")
holiday_dummy <- read_rds("holiday_dummy.rds")
holiday_dummy <- holiday_dummy %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.integer)

incident_holiday <- holiday_dummy %>% bind_cols(incident_all)

# time series cross validation
f_horizon <- fh <- 35
n_init <- length(incident_holiday %>%
  filter_index(. ~ "2018-07-30") %>% pull(date) %>% unique())
train_tscv <- incident_holiday %>%
  filter_index(. ~ "2019-06-26") %>%
  stretch_tsibble(.init = n_init, .step = 1) %>%
  as_tibble() %>%
  select(-date)

test_tscv <- incident_holiday %>%
  filter_index("2018-07-31" ~ .) %>%
  slide_tsibble(.size = f_horizon, .step = 1) %>%
  as_tibble() %>%
  select(-date)

train_nested <- train_tscv %>% nest(data_train = c(1:(ncol(train_tscv) - 1)))
test_nested <- test_tscv %>% nest(data_test = c(1:(ncol(train_tscv) - 1)))

# train <- unnest(train_nested[1,],cols = c(data_train)) %>%  select(-.id)
# test <- unnest(test_nested[1,],cols = c(data_test)) %>%  select(-.id)

all_nested <- train_nested %>%
  bind_cols(test_nested %>% select(-.id))

result <- all_nested

# parallel furrr
library(tictoc)
library(furrr)
plan(multisession, workers = 40)
tic()
result$fct_all <- future_map2(result$data_train, result$data_test, fcst_inciddent)
toc()
write_rds(result, "hts_result_arima.rds")
