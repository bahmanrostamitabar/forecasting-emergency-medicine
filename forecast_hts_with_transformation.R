library(hts)
library(readr)
# object = gts object
# h = forecast horizon
# model_function = function used to model each time series. e.g., ets or auto.arima
# lambda = Box-Cox transformation parameter
# level = prediction interval coverage as a percentage
# method = reconcilation method. either "wls" or "bu"
# nsim = number of simulations in computing prediction intervals using nonparametric method
# htsplus = hts with BoxCox transformation
htsplus <- function(object, h,
                    model_function = auto.arima,
                    lambda=NULL, level=95,
                    method = c("wls","bu"), nsim=2000) {
  method <- match.arg(method)
  # Generate all time series at all levels
  ally <- aggts(object)
  nseries <- NCOL(ally)
  # Do Box-Cox transformation if required
  if(!is.null(lambda)) {
    ally <- BoxCox(ally, lambda)
  }
  # Set up matrices to store results
  fmean <- fvar <- flower <- fupper <- matrix(NA, nrow = h, ncol = nseries)
  models <- list()
  # Loop over all time series and compute forecast means and variances
  for(i in seq(NCOL(ally))) {
    models[[i]] <- model_function(ally[,i])
    fc <- forecast(models[[i]], h=h, level=level)
    fmean[,i] <- fc$mean
    fvar[,i] <- ((fc$upper - fc$lower)/(2*qnorm(0.5+level/200)))^2
  }
  # Reverse Box-Cox transformation if required
  # using Taylor series approximation (delta method) to get mean and variance
  # on original scale
  if(!is.null(lambda)) {
    fmean <- InvBoxCox(fmean, lambda)
    tmp <- lambda * fmean + 1
    fp2 <- fmean^(2-2*lambda)
    fmean <- fmean * (1 + 0.5*fvar*(1-lambda)/(fmean)^(2*lambda))
    fvar <- fvar * fp2
  }
  # S matrix
  S <- smatrix(object)
  # Find G matrix
  if(method == "wls") {
    Lambda <- diag(1/fvar[1,])
    G <- solve(t(S) %*% Lambda %*% S) %*% t(S) %*% Lambda
  } else {
    # Bottom up
    nbottom <- NCOL(object$bts)
    G <- cbind(matrix(0, nrow=nbottom, ncol=nseries-nbottom), diag(nrow=nbottom))
  }
  # Now compute ytilde and its variance
  SG <- S %*% G
  ytilde <- fmean %*% t(SG)
  vartilde <- ytilde * NA
  for(i in seq(h)) {
    vartilde[i,] <- diag(SG %*% diag(fvar[i,]) %*% t(SG))
  }
  # Compute prediction intervals using nonparametric method if lambda not null
  if(is.null(lambda)) {
    # Use normally distributed results
    #note for myself:
    # 1.what is 200?, can we change 0.5 to get different quantiles?
    # 2. If we use other distributions, like poisson
      # 2.1. lower <- qpois(0.025, lambda = ytilde)
      # 2.2. upper <- qpois(0.975, lambda = ytilde)
    flower <- qnorm(0.5 - level/200, mean=ytilde, sd=sqrt(vartilde))
    fupper <- qnorm(0.5 + level/200, mean=ytilde, sd=sqrt(vartilde))
  } else {
    sim <- array(0, c(nseries,h,nsim))
    for(i in seq(nseries)) {
      for(j in seq(nsim)) {
        sim[i,,j] <- simulate(models[[i]], nsim=h)
      }
    }
    sim <- InvBoxCox(sim, lambda)
    for(j in seq(nsim)) {
      sim[,,j] <- SG %*% sim[,,j]
    }
    flower <- t(apply(sim, c(1,2), quantile, prob=0.5 - level/200))
    fupper <- t(apply(sim, c(1,2), quantile, prob=0.5 + level/200))
  }
  # Return mean, variance and lower and upper limits of prediction interval
  list(mean = ytilde, var = vartilde, lower = flower, upper = fupper)
}

# Test on indecent data
incident_all <- read_rds("data/incident_all.rds")
incident_ts <- ts((incident_all), frequency = 7)
#create hierarchical/grouped time series
gts_incident <- hts::gts(incident_ts , 
                               characters = list(c(1,2),3,9))
fcst_bu <- htsplus(gts_incident, h=10, model_function = ets, lambda=0.5,method="bu")
fcst_wls <- htsplus(gts_incident, h=10, model_function = ets, lambda=0.5,method="wls")
