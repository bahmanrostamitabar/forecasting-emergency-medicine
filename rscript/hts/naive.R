# Specific naive model used for this data
naiveglm <- function(y) {
  n <- length(y)
  object <- glm(y ~ 1, family = poisson())
  # Slim down return object and add ts attributes
  object <- list(
    coefficients = object$coefficients,
    residuals = residuals(object, type = "response"),
    y = y
  )
  return(structure(object, class = "naiveglm"))
}

simulate.naiveglm <- function(object, innov, ...) {
  n <- length(object$y)
  h <- NROW(innov)
  # Find mean of future periods
  mean_future <- exp(coefficients(object))
  # Return Poisson series with this mean
  ts(rpois(h, lambda = mean_future),
    frequency = frequency(object$y),
    start = tsp(object$y)[2] + 1 / frequency(object$y)
  )
}
