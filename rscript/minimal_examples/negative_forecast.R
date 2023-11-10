library(hts)


# functions
tscount_f <- function(x, h) {
  xFourier <- forecast::fourier(
    msts(x,
      seasonal.periods = c(7, 365.25)
    ),
    K = c(3, 10)
  )
  xreg_holiday_fourier <- cbind(
    xFourier,
    holiday_dummy_matrix_train
  )
  tsglm_fit <- tscount::tsglm(as.ts(x),
    model = list(past_obs = 1),
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
  pf
}
set.seed(2021)
fh <- 35
holiday_dummy_matrix <- holiday_dummy %>%
  as_tibble() %>%
  select(-date) %>%
  as.matrix()
holiday_dummy_matrix_train <- holiday_dummy_matrix[1:(1400 - fh), ]
holiday_dummy_matrix_test <- holiday_dummy_matrix[((1400 - fh + 1):1400), ]

# Bottom level series
# bts <- msts(matrix(rnorm(2800, 0,.1), ncol=2), seasonal.periods=c(7,365.25))
bts <- ts(matrix(rnorm(2800, 0, .1), ncol = 2))
bts[bts < 0] <- 0
bts <- round(bts)
bts_train <- bts[1:(1400 - fh), ]
bts_test <- bts[((1400 - fh + 1):1400), ]
# Create hts object
y_bts <- hts(bts_train)
plot(y_bts)

# Forecast with ets and arima
comb_ets <- forecast(y_bts,
  h = fh,
  method = "comb",
  fmethod = "ets",
  parallel = TRUE
)
bu_ets <- forecast(y_bts,
  h = fh, method = "bu",
  fmethod = "ets",
  parallel = TRUE
)
comd_arima <- forecast(y_bts,
  h = fh,
  method = "comb",
  fmethod = "arima",
  parallel = TRUE
)
bu_arima <- forecast(y_bts,
  h = fh, method = "bu",
  fmethod = "arima",
  parallel = TRUE
)
aggts(comb_ets)
aggts(bu_ets)
# Create aggregated series
ally <- aggts(y_bts)
# Produce forecasts of all series

allf_stl <- allf_tscount <- matrix(NA, nrow = fh, ncol = NCOL(ally))

for (i in seq(NCOL(ally))) {
  allf_tscount[, i] <- tscount_f(as.vector(ally[, i]), h = fh)
}

allf[allf_tscount < 0] <- 0
# Reconcile results
y.f <- combinef(allf_tscount,
  nodes = get_nodes(y_bts),
  nonnegative = TRUE
)
y.f_all <- aggts(y.f)
y.f_all[y.f_all < 0]
