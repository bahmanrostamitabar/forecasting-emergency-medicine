# Specific tsglm model used for this data
iglm <- function(y) {
  n <- length(y)
  simple_fit <- mean(y > 0) < 0.05
  if(!simple_fit) {
    fourier_year <- forecast::fourier(ts(y, frequency = 365.25), K = 3)
    season_week <- forecast::seasonaldummy(y)
    trend <- splines::ns(seq(n), df = round(n / 100))
    X <- cbind(trend, season_week, fourier_year, holidays[seq_along(y), ])
    # Remove constant covariates
    constant <- apply(X, 2, forecast:::is.constant)
    X <- X[, !constant]
    object <- suppressWarnings(try(glm(y ~ X, family = poisson()), silent = TRUE))
    if (inherits(object, "try-error")) {
      simple_fit <- TRUE
    }
  }
  if(simple_fit) {
    object <- glm(y ~ 1, family = poisson())
  }
  # Slim down return object and add ts attributes
  object <- list(
    coefficients = object$coefficients,
    residuals = residuals(object, type = "response"),
    y = y
  )
  return(structure(object, class = "iglm"))
}

simulate.iglm <- function(object, innov, ...) {
  n <- length(object$y)
  h <- NROW(innov)
  fourier_year <- forecast::fourier(ts(seq(n), frequency = 365.25), K = 3, h = h)
  season_week <- forecast::seasonaldummy(ts(seq(n), frequency = 7), h = h)
  # Use last value of trend for future
  lasttrend <- tail(splines::ns(seq(n), df = round(n / 100)), 1)
  trend <- matrix(rep(lasttrend, h), nrow = h, byrow = TRUE)
  colnames(trend) <- colnames(lasttrend)
  X <- cbind(rep(1, h), trend, season_week, fourier_year, holidays[n + seq(h), ])
  colnames(X) <- c("(Intercept)", paste0("X", colnames(X[, -1])))
  # Remove missing columns
  X <- X[, colnames(X) %in% names(coefficients(object)), drop = FALSE]
  # Find mean of future periods
  mean_future <- c(exp(X %*% matrix(coefficients(object), ncol = 1)))
  # Return Poisson series with this mean
  ts(rpois(h, lambda = mean_future),
    frequency = frequency(object$y),
    start = tsp(object$y)[2] + 1 / frequency(object$y)
  )
}
