library(tidyverse)

hts_result_ets <- read_rds("result/ets_hts_rmsse_long_h.rds")
hts_result_arima <- read_rds("result/arima_hts_rmsse_long_h.rds")
hts_result_tscount <- read_rds("result/tscount_hts_rmsse_long_h.rds")

f_ets <- read_rds("result/forecast/forecast_ets.rds")
f_arima <- read_rds("result/forecast/forecast_arima.rds")
f_tscount <- read_rds("result/forecast/forecast_tscount.rds")

f1 <- f_ets %>%
  filter(.id %in% c(1, 120, 332)) %>%
  select(Total, .id, approach)
f2 <- f_arima %>%
  filter(.id %in% c(1, 120, 332)) %>%
  select(Total, .id, approach)
f3 <- f_tscount %>%
  select(Total, .id, approach) %>%
  filter(Total > 4000)
View(f1)
View(f2)
View(f3)

test <- read_rds("result/test/test.rds")
t1 <- test %>%
  filter(.id %in% c(1, 2, 3)) %>%
  select(Total, .id, approach)
View(t1)
all_hts_result <- hts_result_ets %>%
  bind_rows(hts_result_arima) %>%
  bind_rows(hts_result_tscount)
ets_hts_rmsse_long_h %>%
  filter(approach == "ets_comb", .id == 1 | .id == 2, level == "Total") %>%
  View()


write_rds(all_hts_result, "result/all_hts_result.rds")
all_hts_result %>%
  filter(is.na(error2_scale)) %>%
  View()



rm <- all_hts_result %>% filter(!is.nan(error2_scale))
rm_noninf <- rm %>% filter(!is.infinite(error2_scale))

rmss_hts <- rm_noninf %>%
  group_by(approach, .id, level) %>%
  summarise(rmsse = sqrt(mean(error2_scale)))

rmss_hts %>%
  group_by(approach) %>%
  summarise(rmsse_mean = mean(rmsse), rmsse_median = median(rmsse))

# check

forecast_tscount <- read_rds("result/forecast/forecast_tscount.rds")

forecast_ets <- read_rds("result/forecast/forecast_ets.rds")

forecast_arima <- read_rds("result/forecast/forecast_arima.rds")
forecast_tscount12 <- forecast_tscount %>%
  select(Total, .id, approach) %>%
  filter(.id == 1 | .id == 2)
forecast_ets12 <- forecast_ets %>%
  select(Total, .id, approach) %>%
  filter(.id == 1 | .id == 2)
forecast_arima12 <- forecast_arima %>%
  select(Total, .id, approach) %>%
  filter(.id == 1 | .id == 2)
View(forecast_tscount12)
View(forecast_ets12)
View(forecast_arima12)
