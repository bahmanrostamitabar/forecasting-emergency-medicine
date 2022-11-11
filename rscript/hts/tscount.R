library(tscount)

# Specific tsglm model used for this data
tscount <- function(y) {
  n <- length(y)
  simple_fit <- mean(y > 0) < 0.5
  if (!simple_fit) {
    fourier_year <- forecast::fourier(ts(y, frequency = 365.25), K = 3)
    season_week <- forecast::seasonaldummy(y)
    trend <- splines::ns(seq(n), df = round(n / 300))
    X <- cbind(trend, season_week, fourier_year, holidays[seq_along(y), ])
    # Remove constant covariates
    constant <- apply(X, 2, forecast:::is.constant)
    X <- X[, !constant]
    object <- suppressWarnings(try(tsglm(y, model = list(past_obs = 1:3), xreg = X, link = "log")))
    if (inherits(object, "try-error")) {
      simple_fit <- TRUE
    }
  }
  if (simple_fit) {
    object <- tsglm(y, link = "log")
  }
  # Slim down return object and add ts attributes
  object <- list(
    coefficients = object$coefficients,
    residuals = residuals(object, type = "response"),
    y = y
  )
  return(structure(object, class = "tsglm"))
}

simulate.tsglm <- function(object, innov, ...) {
  n <- length(object$response)
  h <- NROW(innov)
  fourier_year <- forecast::fourier(ts(seq(n), frequency = 365.25), K = 3, h = h)
  season_week <- forecast::seasonaldummy(ts(seq(n), frequency = 7), h = h)
  # Use last value of trend for future
  lasttrend <- tail(splines::ns(seq(n), df = round(n / 300)), 1)
  trend <- matrix(rep(lasttrend, h), nrow = h, byrow = TRUE)
  colnames(trend) <- colnames(lasttrend)
  X <- cbind(trend, season_week, fourier_year, holidays[n + seq(h), ])
  # Remove missing columns
  X <- X[, colnames(X) %in% names(coefficients(object))]
  if (NCOL(X) == 0) {
    # Simple fit
    output <- rpois(h, exp(object$coefficients))
  } else {
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
  }
  ts(output,
    frequency = frequency(object$response),
    start = tsp(object$response)[2] + 1 / frequency(object$response)
  )
}
