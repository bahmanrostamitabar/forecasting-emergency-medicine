library(tidyverse)
library(purrr)
library(hts)
library(forecast)
library(tscount)
library(tsibble)
library(prophet)


#---- prophet parameter setting -------
prophetfit_control <- prophet(
  yearly.seasonality = FALSE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = holidays_p,
  interval.width = 0.95,
  mcmc.samples = 0, # invokes rstan::optimizing (fast MAP estimation)
)

prophetfit_control <- add_seasonality(prophetfit_control,
  name = "weekly",
  period = 7,
  fourier.order = 3
)

prophetfit_control <- add_seasonality(prophetfit_control,
  name = "yearly",
  period = 365.25,
  fourier.order = 10
)

#-------end of prophet parameter setting-------
#--- Begin of function to generate prediction-------
# x is the time series in the training set, h =forecast horizon, date_train=date corresponding to the training,date_test=date corresponding to the test set
prophet_f <- function(x, h, date_train, date_test) {
  df <- data.frame(date_train,
    y = x
  )
  df$y <- BoxCox(df$y, lambda = 0.5) # use Box-Cox transformation to avoid negarive forecast
  prophetfit <- fit.prophet(
    m = prophetfit_control,
    df = df
  )

  prophetfitted <- predict(
    prophetfit,
    date_test
  )
  point_forecast <- InvBoxCox(prophetfitted$yhat, lambda = 0.5)
  # sigma <- (InvBoxCox(prophetfitted$yhat_upper,.5)-InvBoxCox(prophetfitted$yhat_lower,.5))/2/qnorm(0.975), Can we reconcile sd?
  lower <- InvBoxCox(prophetfitted$yhat_lower, lambda = 0.5)
  upper <- InvBoxCox(prophetfitted$yhat_upper, lambda = 0.5)
  output <- as_tibble(point_forecast) %>%
    bind_cols(as_tibble(lower)) %>%
    bind_cols(as_tibble(upper))
  colnames(output) <- c("point_forecast", "lower", "upper")
  output
}
#--- end of function to generate prediction-------

# prepare data
fh <- 35 # forecast horizon

fcst_prophet_inciddent <- function(train, test) {
  incident_all_train <- train[, 2:ncol(train)]
  incident_all_test <- test[, 2:ncol(test)]
  date_train <- train %>%
    select(date) %>%
    rename(ds = date)
  date_test <- test %>%
    select(date) %>%
    rename(ds = date)
  incident_ts_train <- ts(incident_all_train, frequency = 7)
  # create hierarchical/grouped time series
  gts_incident_train <- hts::gts(incident_ts_train,
    characters = list(c(1, 2), 3, 9)
  )
  incident_ts_test <- ts(incident_all_test, frequency = 7)
  gts_incident_test <- hts::gts(incident_ts_test,
    characters = list(c(1, 2), 3, 9)
  )
  # ally <- aggts(gts_incident_train)
  # allf <- matrix(NA,nrow = fh,ncol = ncol(ally))
  # colnames(allf) <- colnames(ally)
  # for(i in 1:ncol(ally))
  #   allf[,i] <- prophet_f(as.vector(ally[,i]),h = fh)
  # produce point forecast, lower and upper bound
  ally_prophet <- aggts(gts_incident_train)
  all_prophetpoint <- matrix(NA, nrow = fh, ncol = ncol(ally_prophet))
  all_prophetlower <- matrix(NA, nrow = fh, ncol = ncol(ally_prophet))
  all_prophetupper <- matrix(NA, nrow = fh, ncol = ncol(ally_prophet))

  for (i in 1:ncol(ally_prophet)) {
    out <- prophet_f(ally_prophet[, i], h = fh, date_train, date_test)
    all_prophetpoint[, i] <- out$point_forecast
    all_prophetlower[, i] <- out$lower
    all_prophetupper[, i] <- out$upper
  }
  # can we use combinef with lower and upper bounds as we do with point forecast
  fct_prophet_comd <- combinef(all_prophetpoint,
    groups = get_groups(gts_incident_train),
    parallel = TRUE,
    nonnegative = TRUE
  )
  # ? How we do BU similar to combinef
  # use bottom level forecast and then follow the way you created series
  prophet_base <- all_prophetpoint[, 534:ncol(all_prophetpoint)]
  colnames(prophet_base) <- colnames(incident_all_train)
  fct_prophet_bu <- hts::gts(prophet_base,
    characters = list(c(1, 2), 3, 9)
  )

  prophet_bu_all <- aggts(fct_prophet_bu)
  prophet_comd_all <- aggts(fct_prophet_comd)
  colnames(prophet_comd_all) <- colnames(prophet_bu_all)

  prophet_c <- as_tibble(prophet_comd_all) %>% bind_cols(approach = rep("prophet_comb", fh))
  prophet_bu <- as_tibble(prophet_bu_all) %>% bind_cols(approach = rep("prophet_bu", fh))

  fcst_all <- prophet_c %>% bind_rows(prophet_bu)


  test_set <- as_tibble(aggts(gts_incident_test))

  prophet_comd_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(prophet_comd_all)) %>% bind_cols(approach = rep("prophet_comb", fh))
  prophet_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(prophet_bu_all)) %>% bind_cols(approach = rep("prophet_bu", fh))

  error <- prophet_comd_e %>% bind_rows(prophet_bu_e)

  prophet_comd_test <- test_set %>% bind_cols(approach = rep("prophet_comb", fh))
  prophet_bu_test <- test_set %>% bind_cols(approach = rep("prophet_bu", fh))

  test_data_all <- prophet_comd_test %>% bind_rows(prophet_bu_test)

  return(list(
    error_all_level = error,
    forecast_all_level = fcst_all,
    testset_all_level = test_data_all
  ))
}


# prepare data
incident_all <- read_rds("incident_all.rds")
holidays_p <- read_rds("holiday_prophet.rds")
holiday_dummy <- read_rds("holiday_dummy.rds")
holiday_dummy <- holiday_dummy %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.integer)

incident <- holiday_dummy %>%
  bind_cols(incident_all) %>%
  select(-public_holiday_d, -school_holiday_d, -xmas, -new_years_day)

#--- time series cross validation series ------
f_horizon <- fh <- 35
n_init <- length(incident %>%
  filter_index(. ~ "2018-07-30") %>% pull(date) %>% unique())
train_tscv <- incident %>%
  filter_index(. ~ "2019-06-26") %>%
  stretch_tsibble(.init = n_init, .step = 1) %>%
  as_tibble()

test_tscv <- incident %>%
  filter_index("2018-07-31" ~ .) %>%
  slide_tsibble(.size = f_horizon, .step = 1) %>%
  as_tibble()

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
result$fct_all <- future_map2(result$data_train, result$data_test, fcst_prophet_inciddent)
toc()
write_rds(result, "hts_result_tscount.rds")
