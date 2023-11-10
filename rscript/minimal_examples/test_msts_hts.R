library(hts)
set.seed(2021)
# Bottom level series
bts <- msts(matrix(rnorm(1500), ncol = 2), seasonal.periods = c(7, 365.25))
bts[bts < 0] <- 0
bts <- round(bts)

# Create hts object
y <- hts(bts)
# plot(y)
# Create aggregated series
ally <- aggts(y)
# Produce forecasts of all series
h <- 28
allf <- matrix(NA, nrow = h, ncol = NCOL(ally))
for (i in seq(NCOL(ally))) {
  allf[, i] <- stlf(ally[, i], h = h)$mean
}
allf[allf < 0] <- 0
# Reconcile results
y.f <- combinef(allf,
  nodes = get_nodes(y),
  nonnegative = TRUE
)
y.f_all <- aggts(y.f)
y.f_all[y.f_all < 0]

# Add back multiple seasonality attributes
y.f$bts <- msts(y.f$bts,
  start = tsp(y$bts)[2] + 1 / tsp(y$bts)[3],
  seasonal.periods = attributes(y$bts)$msts
)
plot(y.f)
