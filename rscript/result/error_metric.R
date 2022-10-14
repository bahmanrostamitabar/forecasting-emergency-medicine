

d <- .train %>% features(incident, unitroot_ndiffs)
D <- .train %>% features(incident, unitroot_nsdiffs)

RMSSE <- function (.error, .train, na.rm = TRUE) 
{
    .train <- diff(.train, lag = .period, differences = D)
  scale <- mean(.train^2, na.rm = na.rm)
  sqrt(mean(.error^2/scale, na.rm = na.rm))
}

