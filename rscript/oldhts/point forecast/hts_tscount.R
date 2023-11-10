library(tidyverse)
library(purrr)
library(hts)
library(forecast)
library(tscount)
library(tsibble)
#---------------- tscount_f----
tscount_f <- function(x, h, holiday_dummy_matrix_train, holiday_dummy_matrix_test) {
  if ((sum(x > 0)) > 20) {
    xFourier <- forecast::fourier(
      msts(x,
        seasonal.periods = c(7, 365.25)
      ),
      K = c(3, 10)
    )
    xreg_holiday_fourier <- cbind(xFourier, holiday_dummy_matrix_train)
    # count non-zeros and depending on the length, fit a simple model like mean
    tsglm_fit <- tsglm(as.ts(x),
      model = list(past_obs = 13),
      link = "log",
      distr = "poisson",
      xreg = xreg_holiday_fourier
    )
    fFourier <- forecast::fourier(msts(x,
      seasonal.periods = c(7, 365.25)
    ), K = c(3, 10), h = fh)
    newxreg_holiday_fourier <- cbind(fFourier, holiday_dummy_matrix_test)
    pf <- predict(tsglm_fit,
      n.ahead = fh,
      newxreg = newxreg_holiday_fourier,
      level = 0
    )$p
  } else {
    pf <- meanf(x, fh)
  }
  pf
}
#---------------- tscount_f----
fcst_inciddent <- function(train, test) {
  incident_all_train <- train %>% select(-public_holiday_d, -school_holiday_d, -xmas, -new_years_day)
  incident_all_test <- test %>% select(-public_holiday_d, -school_holiday_d, -xmas, -new_years_day)
  holiday_dummy_matrix_train <- train %>%
    select(public_holiday_d, school_holiday_d, xmas, new_years_day) %>%
    as.matrix()
  holiday_dummy_matrix_test <- test %>%
    select(public_holiday_d, school_holiday_d, xmas, new_years_day) %>%
    as.matrix()

  incident_ts_train <- ts(incident_all_train, frequency = 7)
  # create hierarchical/grouped time series
  gts_incident_train <- hts::gts(incident_ts_train,
    characters = list(c(1, 2), 3, 9)
  )
  incident_ts_test <- ts(incident_all_test, frequency = 7)
  gts_incident_test <- hts::gts(incident_ts_test,
    characters = list(c(1, 2), 3, 9)
  )
  ally <- aggts(gts_incident_train)
  allf <- matrix(NA, nrow = fh, ncol = ncol(ally))
  colnames(allf) <- colnames(ally)
  for (i in 1:ncol(ally)) {
    allf[, i] <- tscount_f(as.vector(ally[, i]),
      h = fh, holiday_dummy_matrix_train,
      holiday_dummy_matrix_test
    )
  }


  # check this for very low volume series
  forecast_strange <- as_tibble(allf) %>% select(NBCREDSTROKECVA)
  train_strange <- as_tibble(ally) %>% select(NBCREDSTROKECVA)
  test_strange <- as_tibble(aggts(gts_incident_test)) %>% select(NBCREDSTROKECVA)
  write_rds(forecast_strange, "forecast_strange.rds")
  write_rds(train_strange, "train_strange.rds")
  write_rds(test_strange, "test_strange.rds")

  fct_tscount_comd <- combinef(allf,
    groups = get_groups(gts_incident_train),
    parallel = TRUE,
    nonnegative = TRUE
  )

  # 534: start of bottom series
  tscount_base <- allf[, 534:ncol(allf)]
  colnames(tscount_base) <- colnames(incident_all_train)
  fct_tscount_bu <- hts::gts(tscount_base,
    characters = list(c(1, 2), 3, 9)
  )

  tscount_bu_all <- aggts(fct_tscount_bu)

  tscount_comd_all <- aggts(fct_tscount_comd)
  colnames(tscount_comd_all) <- colnames(tscount_bu_all)

  tscount_c <- as_tibble(tscount_comd_all) %>% bind_cols(approach = rep("tscount_comb", fh))
  tscount_bu <- as_tibble(tscount_bu_all) %>% bind_cols(approach = rep("tscount_bu", fh))

  fcst_all <- tscount_c %>% bind_rows(tscount_bu)


  test_set <- as_tibble(aggts(gts_incident_test))

  tscount_comd_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(tscount_comd_all)) %>% bind_cols(approach = rep("tscount_comb", fh))
  tscount_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(tscount_bu_all)) %>% bind_cols(approach = rep("tscount_bu", fh))

  error <- tscount_comd_e %>% bind_rows(tscount_bu_e)

  tscount_comd_test <- test_set %>% bind_cols(approach = rep("tscount_comb", fh))
  tscount_bu_test <- test_set %>% bind_cols(approach = rep("tscount_bu", fh))

  test_data_all <- tscount_comd_test %>% bind_rows(tscount_bu_test)

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
t <- result[1, ]
train <- t$data_train[[1]]
test <- t$data_test[[1]]
library(tictoc)
library(furrr)
plan(multisession, workers = 40)
tic()
result$fct_all <- future_map2(result$data_train, result$data_test, fcst_inciddent)
toc()
write_rds(result, "hts_result_tscount.rds")
