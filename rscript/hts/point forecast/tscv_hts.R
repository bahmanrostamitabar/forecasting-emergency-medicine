library("opera")
# Parallel Setting
library("parallel")
library("doParallel")
library("foreach")
library("forecast")
library(tidyverse)
library(hts)
library(forecast)
library(tscount)

incident_all <- read_rds("incident_all.rds")
#holidays_p <- read_rds("data/holiday_prophet.rds")
holiday_dummy <- read_rds("holiday_dummy.rds")
holiday_dummy <- holiday_dummy %>% 
  mutate_if(is.factor,as.character)%>% 
  mutate_if(is.character, as.integer)
holiday_dummy_matrix <- holiday_dummy %>% as_tibble() %>% select(-date) %>% as.matrix()
#Functions
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

k <- 1035 # minimum data length for fitting a model
fh <- 35
n <- 1400# nrow(incident_all)

#for(i in 1:(n-(k+fh-1)))
#for(i in (1:2))
  #start_t <- Sys.time()
cores <- detectCores(logical = FALSE)
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cores=cl) # multicore-like functionality
getDoParWorkers()
getDoParName()
getDoParVersion()
# Compute forecast and error
Container <- foreach (i = 1:2, .combine=rbind, .packages="opera") %dopar% {
  holiday_dummy_matrix_train <- holiday_dummy_matrix[1:(k+i-1),]
  holiday_dummy_matrix_test <- holiday_dummy_matrix[((k+i):(k+i+fh-1)),]
  
  incident_all_train <- incident_all[1:(k+i-1),]
  incident_all_test <- incident_all[((k+i):(k+i+fh-1)),]
  
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
  tscount_base <- allf[,534:ncol(allf)]
  colnames(tscount_base) <- colnames(incident_all_train)
  fct_tscount_bu <- hts::gts(tscount_base , 
                             characters = list(c(1,2),3,9))
  
  #How to get error for the forecast horizon: 1) use aggts(comd_arima) - gts_incident_test
  #accuracy.gts(comd_arima, gts_incident_test)[c(1,2,6),]
  ets_comd_all <- (aggts(comb_ets))^2
  ets_bu_all <- (aggts(bu_ets))^2
  # ets_comd_all[ets_comd_all<0] <- 0
  # ets_bu_all[ets_bu_all<0] <- 0
  
  tscount_comd_all <- aggts(fct_tscount_comd)
  colnames(tscount_comd_all) <- colnames(ets_comd_all)
  tscount_bu_all <- aggts(fct_tscount_bu)
  # tscount_comd_all[tscount_comd_all<0] <- 0
  # tscount_bu_all[tscount_bu_all<0] <- 0
  
  arima_comd_all <- (aggts(comd_arima))^2
  arima_bu_all <- (aggts(bu_arima))^2
  # arima_comd_all[arima_comd_all<0] <- 0
  # arima_bu_all[arima_bu_all<0] <- 0
  
  
  tscount_c <- as_tibble(tscount_comd_all) %>% bind_cols(approach = rep("tscount_comb", fh)) 
  ets_c <- as_tibble(ets_comd_all) %>% bind_cols(approach = rep("ets_comb", fh))
  arima_c <- as_tibble(arima_comd_all) %>% bind_cols(approach = rep("arima_comb", fh))
  
  tscount_bu <- as_tibble(tscount_bu_all) %>% bind_cols(approach = rep("tscount_bu", fh))
  ets_bu <- as_tibble(ets_bu_all) %>% bind_cols(approach = rep("ets_bu", fh))
  arima_bu <- as_tibble(arima_bu_all) %>% bind_cols(approach = rep("arima_bu", fh))
  
  
all_approach <- tscount_c %>% bind_rows(ets_c) %>% bind_rows(arima_c) %>% 
    bind_rows(tscount_bu) %>% bind_rows(ets_bu) %>% bind_rows(arima_bu)

fcst_all <- all_approach %>% bind_cols(.id = rep(i, nrow(all_approach))) %>%
  select(.id, approach, everything())

#test_all <- as_tibble(aggts(gts_incident_test))  %>% bind_rows(as_tibble(aggts(gts_incident_test))) %>% bind_rows(as_tibble(aggts(gts_incident_test))) %>% 
#    bind_rows(as_tibble(aggts(gts_incident_test))) %>% bind_rows(as_tibble(aggts(gts_incident_test))) %>% bind_rows(as_tibble(aggts(gts_incident_test)))
  
test_set <- as_tibble(aggts(gts_incident_test)) %>% bind_cols(.id = rep(i, nrow(as_tibble(aggts(gts_incident_test))))) %>%
  select(.id, everything())
  
  tscount_comd_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(tscount_comd_all)) %>% bind_cols(approach = rep("tscount_comb", fh))
  tscount_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(tscount_bu_all)) %>% bind_cols(approach = rep("tscount_bu", fh))
  
  arima_comb_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(arima_comd_all)) %>% bind_cols(approach = rep("arima_comb", fh))
  arima_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(arima_bu_all)) %>% bind_cols(approach = rep("arima_bu", fh))
  
  ets_comb_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(ets_comd_all)) %>% bind_cols(approach = rep("ets_comb", fh))
  ets_bu_e <- (as_tibble(aggts(gts_incident_test)) - as_tibble(ets_comd_all)) %>% bind_cols(approach = rep("ets_bu", fh))
  
error <- tscount_comd_e %>% bind_rows(tscount_bu_e) %>% bind_rows(arima_comb_e) %>% 
    bind_rows(arima_bu_e) %>% bind_rows(ets_comb_e) %>% bind_rows(ets_bu_e)
  
all_error <- error %>% bind_cols(.id = rep(i, nrow(error))) %>%
  select(.id, approach, everything())

return(list(error_all_level=all_error,
            forecast_all_level=fcst_all,
            testset_all_level=test_set))
}

error_all_level <- bind_rows(Container[,1],.id = ".id")
forecast_all_level <- bind_rows(Container[,2],.id = ".id")
testset_all_level <- bind_rows(Container[,3],.id = ".id")

saveRDS(error_all_level,file="error_all_level.rds")
saveRDS(forecast_all_level,file="error_all_level.rds")
saveRDS(testset_all_level,file="error_all_level.rds")