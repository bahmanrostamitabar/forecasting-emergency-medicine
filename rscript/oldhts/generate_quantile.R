library(forecast)
# Normal distribution
fit <- auto.arima(WWWusage)
fc <- forecast(fit, h = 20, level = 95)
qf <- matrix(0, nrow = 19, ncol = 20)
m <- fc$mean
s <- (fc$upper - fc$lower) / 1.96 / 2
for (h in 1:20) {
  qf[, h] <- qnorm(seq(0.05, .95, .05), m[h], s[h])
}

plot(fc)
matlines(101:120, t(qf), col = rainbow(120), lty = 1)

# poisson
qf <- matrix(0, nrow = 19, ncol = 20)
m <- fc$mean # mean should be the point forecast generate from a forecasting model sy=uch as poinsson regression , tscount , etc
for (h in 1:20) {
  qf[, h] <- qpois(seq(0.05, .95, .05), m[h])
}

# for poisson
lower <- qpois(0.025, lambda = ytilde)
upper <- qpois(0.975, lambda = ytilde)
