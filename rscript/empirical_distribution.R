
# empirical distribution

library(fpp3)
library(distributional)
set.seed(123)

train <- tsibble(t = 1:100, y=rnorm(100), index=t)
test <- tsibble(t = 101:120, y=rnorm(20), index=t)
fc_ar1 <- train %>%
  model(
    ar1 = ARIMA(y ~ pdq(1,0,0))
  ) %>%
  fabletools::forecast(h=20)
fc_benchmark <- fc_ar1 %>% as_tibble() %>% 
  mutate(
    y = distributional::dist_sample(list(train$y)),
    .mean1=mean(train$y),
    .model = 'benchmark'
  )
bind_rows(fc_ar1, fc_benchmark) %>%
  accuracy(test, measures = distribution_accuracy_measures)

