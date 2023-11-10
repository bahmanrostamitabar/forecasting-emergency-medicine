library(tidyverse)
library(hts)
library(forecast)
library(tscount)

fh <- 5 * 7
incident_all <- read_rds("data/incident_all.rds")
holidays_p <- read_rds("data/holiday_prophet.rds")
holiday_dummy <- read_rds("data/holiday_dummy.rds")
holiday_dummy <- holiday_dummy %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.integer)

holiday_dummy_matrix <- holiday_dummy %>%
  as_tibble() %>%
  select(-date) %>%
  as.matrix()
holiday_dummy_matrix_train <- holiday_dummy_matrix[1:(1400 - fh), ]
holiday_dummy_matrix_test <- holiday_dummy_matrix[((1400 - fh + 1):1400), ]

incident_all_train <- incident_all[1:(1400 - fh), ]
incident_all_test <- incident_all[((1400 - fh + 1):1400), ]
#----

# incident_msts_train <- msts(incident_all_train,seasonal.periods = c(7,365.25))
# create hierarchical/grouped time series
incident_ts_train <- ts(incident_all_train, frequency = 7)
gts_incident_train <- hts::gts(incident_ts_train,
  characters = list(c(1, 2), 3, 9)
)
# tets set
# incident_msts_test <- msts(incident_all_test, seasonal.periods = c(7,365.25))
incident_ts_test <- ts(incident_all_test, frequency = 7)
gts_incident_test <- hts::gts(incident_ts_test,
  characters = list(c(1, 2), 3, 9)
)
# this is to create grouped time series
# gts_incident <- hts::gts(incident_msts , characters = c(1,2,3,9))

# Forecast with ets and arima
comb_ets <- forecast(gts_incident_train,
  h = fh,
  method = "comb",
  fmethod = "ets",
  parallel = TRUE
)
bu_ets <- forecast(gts_incident_train,
  h = fh, method = "bu",
  fmethod = "ets",
  parallel = TRUE
)
comd_arima <- forecast(gts_incident_train,
  h = fh,
  method = "comb",
  fmethod = "arima",
  parallel = TRUE
)
bu_arima <- forecast(gts_incident_train,
  h = fh, method = "bu",
  fmethod = "arima",
  parallel = TRUE
)
write_rds(bu_arima, "bu_arima.rds")
write_rds(comd_arima, "comd_arima.rds")
write_rds(bu_ets, "bu_ets.rds")
write_rds(comb_ets, "comb_ets.rds")

bu_arima <- read_rds("bu_arima.rds")
comd_arima <- read_rds("comd_arima.rds")
bu_ets <- read_rds("bu_ets.rds")
comb_ets <- read_rds("comb_ets.rds")

# forecast with tscount
# x <- incident_all_train[,1]
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

incident_msts_train <- msts(incident_all_train, seasonal.periods = c(7, 365.25))
gts_incident_train <- hts::gts(incident_msts_train,
  characters = list(c(1, 2), 3, 9)
)
gts_incident_train <- hts::gts(incident_all_train,
  characters = list(c(1, 2), 3, 9)
)
ally <- aggts(gts_incident_train)
allf <- matrix(NA, nrow = fh, ncol = ncol(ally))

for (i in 1:ncol(ally)) {
  allf[, i] <- tscount_f(as.vector(ally[, i]), h = fh)
}
# Is it an issue if allf does not have column names? View(allf)
# allf_msts <- msts(allf, seasonal.periods=c(7,365.25))
# why combinef does not work with msts? shall we use ts() or a matrix is fine?
# Does combinef uses the information related to frequency of ts or msts?
write_rds(allf, "allf.rds")
allf <- read_rds("allf.rds")
fct_tscount_comd <- combinef(allf,
  groups = get_groups(gts_incident_train),
  parallel = TRUE,
  nonnegative = TRUE
)
colnames(fct_tscount_comd_all) <- colnames(incident_all_train)
fct_tscount_comd_all <- aggts(fct_tscount_comd)
# Add back multiple seasonality attributes
incident_msts_test <- msts(incident_all_test, seasonal.periods = c(7, 365.25))
gts_incident_test <- hts::gts(incident_msts_test,
  characters = list(c(1, 2), 3, 9)
)
fct_tscount_comd$bts <- msts(fct_tscount_comd$bts,
  start = tsp(gts_incident_test$bts)[2] + 1 / tsp(gts_incident_test$bts)[3],
  seasonal.periods = attributes(gts_incident_test$bts)$msts
)
# ? How we do BU similar to combinef
# use bottom level forecast and then follow the way you created series
tscount_base <- allf[, 534:ncol(allf)]
colnames(tscount_base) <- colnames(incident_all_train)
fct_tscount_bu <- hts::gts(tscount_base,
  characters = list(c(1, 2), 3, 9)
)

# Get ts
agg_gts <- aggts(bu_arima)
agg_gts1 <- aggts(bu_arima, levels = 1)

# Acuuracy

accuracy.gts(comd_arima, gts_incident_test) %>% View()
accuracy.gts(bu_arima, gts_incident_test) %>% View()

accuracy.gts(comb_ets, gts_incident_test) %>% View()
accuracy.gts(comb_ets, gts_incident_test) %>% View()

accuracy.gts(fct_tscount_comd, gts_incident_test) %>% View()
accuracy.gts(fct_tscount_bu, gts_incident_test) %>% View()

#---- prophet-------
prophetfit_control <- prophet(
  yearly.seasonality = FALSE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = holidays_p,
  mcmc.samples = 0, # invokes rstan::optimizing (fast MAP estimation)
  uncertainty.samples = 0,
  fit = FALSE
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
prophet_f <- function(x, fh) {
  prophetfit <- fit.prophet(
    m = prophetfit_control,
    df = data.frame(
      ds = date_train,
      y = sqrt(pull(x))
    )
  )

  prophetfitted <- round(predict(
    prophetfit,
    data.frame(ds = date_test)
  )$yhat)
  prophetfitted
}

ally_prophet <- aggts(gts_incident)
all_prophetf <- matrix(NA, nrow = fh, ncol = ncol(ally_prophet))

for (i in 1:ncol(ally_prophet)) {
  all_prophetf[, i] <- prophet_f(ally_prophet[, 1], h = fh)
}
all_prophetf_msts <- msts(all_prophetf, seasonal.periods = c(7, 365.25))
fct_prophet_comd <- combinef(all_prophetf_msts,
  groups = get_groups(gts_incident),
  parallel = TRUE,
  keep = "gts"
)
# ? How we do BU similar to combinef
# use bottom level forecast and then follow the way you created series
fct_prophet_bu <- hts::gts(all_prophetf,
  characters = list(c(1, 2), c(3, 9))
)


# Reproducible example with combinef-------
abc <- ts(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
g <- rbind(c(1, 1, 2, 2), c(1, 2, 1, 2))
y <- gts(abc, groups = g)
h <- 12
ally <- aggts(y)
allf <- matrix(NA, nrow = h, ncol = ncol(ally))
for (i in 1:ncol(ally)) {
  allf[, i] <- tscount_f(ally[, i], h = h)
}
allf <- ts(allf, start = 51)
y.f <- combinef(allf,
  groups = get_groups(y),
  keep = "gts", algorithms = "lu"
)
plot(y.f)
