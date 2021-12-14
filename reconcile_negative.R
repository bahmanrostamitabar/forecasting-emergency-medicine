library(hts)
set.seed(1)
# Bottom level series
bts <- msts(matrix(rnorm(1500), ncol=2), seasonal.periods=c(7,365.25))
bts[bts<0] <- 0
bts <- round(bts)

# Create hts object
y <- hts(bts)
plot(y)
# Create aggregated series
ally <- aggts(y)
# Produce forecasts of all series
h <- 28
allf <- matrix(NA, nrow = h, ncol = NCOL(ally))
for(i in seq(NCOL(ally)))
  allf[,i] <- stlf(ally[,i], h = h)$mean
allf[allf<0] <- 0
# Reconcile results
y.f <- combinef(allf, get_nodes(y))
y.f_all <- aggts(y.f)
