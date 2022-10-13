It is easy enough to do using the equations from the JASA paper. Here is an example using the infantgts data from the hts package.

library(hts)

h <- 10
ally <- aggts(infantgts)
fmean <- fvar <- matrix(NA, nrow = h, ncol = NCOL(ally))
for(i in seq(NCOL(ally))) {
  fc <- forecast(auto.arima(ally[,i]), h = h, level=95)
  fmean[,i] <- fc$mean
  fvar[,i] <- ((fc$upper - fc$lower)/(2*qnorm(0.975)))^2
}
S <- smatrix(infantgts)
Lambda <- diag(1/fvar[1,])
G <- solve(t(S) %*% Lambda %*% S) %*% t(S) %*% Lambda
SG <- S %*% G
ytilde <- SG %*% t(fmean)
vartilde <- ytilde * NA
for(i in seq(h)) {
  vartilde <- diag(SG %*% diag(fvar[i,]) %*% t(SG))
}
lower <- qnorm(0.025, mean = ytilde, sd = sqrt(vartilde))
upper <- qnorm(0.975, mean = ytilde, sd = sqrt(vartilde))

I have computed the mean directly (as ytilde). This should give the same results as 
combinef(fmean, groups = get_groups(infantgts), weights=1/fvar[1,])

But since we needed all the matrices for the variance calculation, I thought it would be more obvious what is happening if I computed ytilde as well.

Because we are using a transformation of the data (sqrt) before fitting the models, you need to compute ytilde, lower and upper on the transformed data. Then square them all to give the results on the original scale.

For the Poisson case, you could simply compute

lower <- qpois(0.025, lambda = ytilde)
upper <- qpois(0.975, lambda = ytilde)

In this case, you don't need a sqrt transformation of the data, and the resulting ytilde, lower and upper are already on the same scale as the data.
If there are negative values of ytilde, they will need to be set to zero.