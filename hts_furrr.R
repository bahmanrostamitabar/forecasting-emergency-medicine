library(tidyverse)
library(purrr)
library(furrr)
library(hts)
library(forecast)
library(tscount)
library(tsibble)
#---------------- tscount_f----
tscount_f <- function(x,h,holiday_dummy_matrix_train,holiday_dummy_matrix_test) {
  xFourier <- forecast::fourier(msts(x,
                                     seasonal.periods=c(7,365.25)),
                                K=c(3,10))
  xreg_holiday_fourier <- cbind(xFourier,holiday_dummy_matrix_train)
  tsglm_fit <- tsglm(as.ts(x),
                     model = list(past_obs = 1), 
                     link = "log", 
                     distr = "poisson",
                     xreg= xreg_holiday_fourier
  )
  fFourier <- forecast::fourier(msts(x,
                                     seasonal.periods=c(7,365.25)),K=c(3,10),h=fh)
  newxreg_holiday_fourier <- cbind(fFourier,holiday_dummy_matrix_test)
  pf <- predict(tsglm_fit, 
                n.ahead = fh, 
                newxreg = newxreg_holiday_fourier,
                level=0
  )$p
  pf
}
#---------------- tscount_f----
#fcst_inciddent() will take train and test data, generate forecast for the test and calculate error for each approach
fcst_inciddent <- function(train, test) {
incident_all_train <- train %>% select(-public_holiday_d,-school_holiday_d,-xmas,-new_years_day)
incident_all_test <- test %>% select(-public_holiday_d,-school_holiday_d,-xmas,-new_years_day)
holiday_dummy_matrix_train <- train %>% select(public_holiday_d,school_holiday_d,xmas,new_years_day) %>% as.matrix()
holiday_dummy_matrix_test <- test %>% select(public_holiday_d,school_holiday_d,xmas,new_years_day) %>% as.matrix()

incident_ts_train <- ts(sqrt(incident_all_train), frequency = 7)
#create hierarchical/grouped time series
gts_incident_train <- hts::gts(incident_ts_train , 
                               characters = list(c(1,2),3,9))
incident_ts_test <- ts(incident_all_test, frequency = 7)
gts_incident_test <- hts::gts(incident_ts_test , 
                              characters = list(c(1,2),3,9))
comb_ets <- forecast(gts_incident_train, 
                     h=fh,
                     method = "comb",
                     fmethod="ets", 
                     parallel = TRUE)
bu_ets <- forecast(gts_incident_train, 
                   h=fh,method="bu", 
                   fmethod="ets", 
                   parallel = TRUE)
comd_arima <- forecast(gts_incident_train, 
                       h=fh,
                       method = "comb",
                       fmethod="arima", 
                       parallel = TRUE)
bu_arima <- forecast(gts_incident_train, 
                     h=fh,method="bu", 
                     fmethod="arima", 
                     parallel = TRUE)

ally <- aggts(gts_incident_train)
allf <- matrix(NA,nrow = fh,ncol = ncol(ally))

for(i in 1:ncol(ally))
  allf[,i] <- tscount_f(as.vector(ally[,i]),h = fh,holiday_dummy_matrix_train, 
                        holiday_dummy_matrix_test)

fct_tscount_comd <- combinef(allf, 
                             groups = get_groups(gts_incident_train),
                             parallel = TRUE,
                             nonnegative = TRUE)

#534: start of bottom series
tscount_base <- allf[,534:ncol(allf)]
colnames(tscount_base) <- colnames(incident_all_train)
fct_tscount_bu <- hts::gts(tscount_base , 
                           characters = list(c(1,2),3,9))

ets_comd_all <- (aggts(comb_ets))^2
ets_bu_all <- (aggts(bu_ets))^2


tscount_comd_all <- aggts(fct_tscount_comd)
colnames(tscount_comd_all) <- colnames(ets_comd_all)
tscount_bu_all <- aggts(fct_tscount_bu)

arima_comd_all <- (aggts(comd_arima))^2
arima_bu_all <- (aggts(bu_arima))^2

tscount_c <- as_tibble(tscount_comd_all) %>% bind_cols(approach = rep("tscount_comb", fh)) 
ets_c <- as_tibble(ets_comd_all) %>% bind_cols(approach = rep("ets_comb", fh))
arima_c <- as_tibble(arima_comd_all) %>% bind_cols(approach = rep("arima_comb", fh))

tscount_bu <- as_tibble(tscount_bu_all) %>% bind_cols(approach = rep("tscount_bu", fh))
ets_bu <- as_tibble(ets_bu_all) %>% bind_cols(approach = rep("ets_bu", fh))
arima_bu <- as_tibble(arima_bu_all) %>% bind_cols(approach = rep("arima_bu", fh))


fcst_all <- tscount_c %>% bind_rows(ets_c) %>% bind_rows(arima_c) %>% 
  bind_rows(tscount_bu) %>% bind_rows(ets_bu) %>% bind_rows(arima_bu)


test_set <- as_tibble(aggts(gts_incident_test))

tscount_comd_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(tscount_comd_all)) %>% bind_cols(approach = rep("tscount_comb", fh))
tscount_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(tscount_bu_all)) %>% bind_cols(approach = rep("tscount_bu", fh))

arima_comb_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(arima_comd_all)) %>% bind_cols(approach = rep("arima_comb", fh))
arima_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(arima_bu_all)) %>% bind_cols(approach = rep("arima_bu", fh))

ets_comb_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(ets_comd_all)) %>% bind_cols(approach = rep("ets_comb", fh))
ets_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(ets_comd_all)) %>% bind_cols(approach = rep("ets_bu", fh))

error <- tscount_comd_e %>% bind_rows(tscount_bu_e) %>% bind_rows(arima_comb_e) %>% 
  bind_rows(arima_bu_e) %>% bind_rows(ets_comb_e) %>% bind_rows(ets_bu_e)

return(list(error_all_level=error,
            forecast_all_level=fcst_all,
            testset_all_level=test_set))
}

#read incident and holiday data
incident_all <- read_rds("data/incident_all.rds")
holiday_dummy <- read_rds("data/holiday_dummy.rds")
holiday_dummy <- holiday_dummy %>% 
  mutate_if(is.factor,as.character)%>% 
  mutate_if(is.character, as.integer)

incident_holiday <- holiday_dummy %>% bind_cols(incident_all)

# time series cross validation
f_horizon <- fh <- 35
n_init <- length(incident_holiday %>% 
                   filter_index(. ~ "2018-07-30") %>% pull(date) %>% unique())
train_tscv <- incident_holiday %>% 
  filter_index(. ~ "2019-06-26") %>% 
  stretch_tsibble(.init = n_init, .step = 6) %>% 
  as_tibble() %>%
  select(-date)

test_tscv <- incident_holiday %>% filter_index("2018-07-31" ~ .) %>%
  slide_tsibble(.size = f_horizon, .step = 6) %>% as_tibble() %>%  select(-date)

#nest
train_nested <- train_tscv %>% nest(data_train=c(1:(ncol(train_tscv)-1)))
test_nested <- test_tscv %>% nest(data_test=c(1:(ncol(train_tscv)-1)))
all_nested <- train_nested  %>% 
  bind_cols(test_nested %>% select(-.id))

#train <- unnest(train_nested[1,],cols = c(data_train)) %>%  select(-.id)
#test <- unnest(test_nested[1,],cols = c(data_test)) %>%  select(-.id)


library(tictoc)
plan(multisession, workers=40)
tic()
result <- all_nested %>% 
  mutate(fct_error=future_map2(data_train,data_test,fcst_inciddent))
toc()

write_rds(result,"hts_result.rds")


