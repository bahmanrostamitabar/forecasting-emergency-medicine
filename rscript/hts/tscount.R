library(tscount)

# Specific tsglm model used for this data
tscount <- function(y) {
  fourier_year <- forecast::fourier(ts(y, frequency = 365.25), K = 3)
  season_week <- forecast::seasonaldummy(y)
  trend <- splines::ns(seq(n), df = round(n / 100))
  X <- cbind(trend, season_week, fourier_year, holidays[seq_along(y), ])
  # Remove constant covariates
  constant <- apply(X, 2, forecast:::is.constant)
  X <- X[, !constant]
  tsglm(y, model = list(past_obs = 1:3), xreg = X, link = "log")
}

simulate.tsglm <- function(object, innov, ...) {
  n <- length(object$response)
  h <- NROW(innov)
  fourier_year <- forecast::fourier(ts(seq(n), frequency = 365.25), K = 3, h = h)
  season_week <- forecast::seasonaldummy(ts(seq(n), frequency = 7), h = h)
  # Use last value of trend for future
  lasttrend <- tail(splines::ns(seq(n), df = round(n / 100)), 1)
  trend <- matrix(rep(lasttrend, h), nrow = h, byrow = TRUE)
  colnames(trend) <- colnames(lasttrend)
  X <- cbind(trend, season_week, fourier_year, holidays[n + seq(h), ])
  # Remove missing columns
  X <- X[, colnames(X) %in% names(coefficients(object))]
  output <- tsglm.sim(h,
    param = list(
      intercept = object$coefficient[1],
      past_obs = object$coefficients[2:4],
      past_mean = FALSE,
      xreg = object$coefficients[-(1:4)]
    ),
    model = list(
      past_obs = 1:3,
      past_mean = FALSE,
      external = FALSE
    ),
    xreg = X,
    link = "log"
  )$ts
  ts(output,
    frequency = frequency(object$response),
    start = tsp(object$response)[2] + 1 / frequency(object$response)
  )
}
