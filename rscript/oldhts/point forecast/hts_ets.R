library(tidyverse)
library(purrr)
library(hts)
library(forecast)
library(tscount)
library(tsibble)
fcst_inciddent <- function(train, test) {
incident_all_train <- train %>% select(-public_holiday_d,-school_holiday_d,-xmas,-new_years_day)
incident_all_test <- test %>% select(-public_holiday_d,-school_holiday_d,-xmas,-new_years_day)
holiday_dummy_matrix_train <- train %>% select(public_holiday_d,school_holiday_d,xmas,new_years_day) %>% as.matrix()
holiday_dummy_matrix_test <- test %>% select(public_holiday_d,school_holiday_d,xmas,new_years_day) %>% as.matrix()

incident_ts_train <- ts((incident_all_train), frequency = 7)
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
                     lambda = .5,
                     parallel = TRUE,
                     )

bu_ets <- forecast(gts_incident_train, 
                   h=fh,method="bu", 
                   fmethod="ets", 
                   lambda = .5,
                   parallel = TRUE
)

ally <- aggts(gts_incident_train)
allf <- matrix(NA,nrow = fh,ncol = ncol(ally))

#How to get error for the forecast horizon: 1) use aggts(comd_arima) - gts_incident_test
#accuracy.gts(comd_arima, gts_incident_test)[c(1,2,6),]
ets_comd_all <- (aggts(comb_ets))
ets_bu_all <- (aggts(bu_ets))

ets_c <- as_tibble(ets_comd_all) %>% bind_cols(approach = rep("ets_comb", fh))
ets_bu <- as_tibble(ets_bu_all) %>% bind_cols(approach = rep("ets_bu", fh))

fcst_all <- ets_c %>% bind_rows(ets_bu) 

test_set <- as_tibble(aggts(gts_incident_test))
ets_comb_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(ets_comd_all)) %>% bind_cols(approach = rep("ets_comb", fh))
ets_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(ets_bu_all)) %>% bind_cols(approach = rep("ets_bu", fh))

error <- ets_comb_e %>% bind_rows(ets_bu_e)
ets_comb_test <- test_set %>% bind_cols(approach = rep("ets_comb", fh))
ets_bu_test <- test_set %>% bind_cols(approach = rep("ets_bu", fh))

test_data_all <- ets_comb_test %>% bind_rows(ets_bu_test)

return(list(error_all_level=error,
            forecast_all_level=fcst_all,
            testset_all_level=test_data_all))
}

incident_all <- read_rds("incident_all.rds")
#holidays_p <- read_rds("data/holiday_prophet.rds")
holiday_dummy <- read_rds("holiday_dummy.rds")
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
  stretch_tsibble(.init = n_init, .step = 1) %>% 
  as_tibble() %>%
  select(-date)

test_tscv <- incident_holiday %>% filter_index("2018-07-31" ~ .) %>%
  slide_tsibble(.size = f_horizon, .step = 1) %>% as_tibble() %>%  select(-date)


train_nested <- train_tscv %>% nest(data_train=c(1:(ncol(train_tscv)-1)))
test_nested <- test_tscv %>% nest(data_test=c(1:(ncol(train_tscv)-1)))

#train <- unnest(train_nested[1,],cols = c(data_train)) %>%  select(-.id)
#test <- unnest(test_nested[1,],cols = c(data_test)) %>%  select(-.id)

all_nested <- train_nested  %>% 
  bind_cols(test_nested %>% select(-.id))
result <- all_nested

#parallel furrr
library(tictoc)
library(furrr)
plan(multisession, workers=40)
tic()
# result <- all_nested %>% 
#   mutate(fct_error=future_map2(data_train,data_test,fcst_inciddent))
result$fct_all <- future_map2(result$data_train,result$data_test,fcst_inciddent)
toc()
write_rds(result,"hts_result_ets.rds")
#forecast
fct <- result$fct_all
id <- tibble(.id=rep(1,70))
a1 <- (fct[[1]]$forecast_all_level) %>% bind_cols(id)
a1[,] <- NA
for (i in (1:nrow(result))) {
  a <- (fct[[i]]$forecast_all_level)
  id <- tibble(.id=rep(i,70))
  a <- a %>% bind_cols(id)
  a1 <- a1 %>% bind_rows(a)
}
forecast_hts <- a1 %>% drop_na()


#766.276 sec elapsed


#no parallel
#11796.309 sec elapsed
library(tictoc)
tic()
result$fct_all <- map2(result$data_train,result$data_test,fcst_inciddent)
toc()
write_rds(result,"hts_result_ets2.rds")

#paraller future
#11847.51 sec elapsed
library(tictoc)
library(future)
plan(multisession, workers=40)
tic()
result$fct_all <- map2(result$data_train,result$data_test,fcst_inciddent)
toc()
write_rds(result,"hts_result_ets1.rds")
