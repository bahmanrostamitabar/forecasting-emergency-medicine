# Specific naive model used for this data
naiveecdf <- function(y) {
  object <- list(
    residuals = y - mean(y),
    y = y
  )
  return(structure(object, class = "naiveecdf"))
}

simulate.naiveecdf <- function(object, innov, ...) {
  n <- length(object$y)
  h <- NROW(innov)
  ts(sample(object$y, size = h, replace = TRUE),
    frequency = frequency(object$y),
    start = tsp(object$y)[2] + 1 / frequency(object$y)
  )
}
