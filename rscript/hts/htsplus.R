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
                    lambda = NULL, level = 95,
                    method = c("wls", "bu", "mint"), nsim = 2000) {
  method <- match.arg(method)
  # Generate all time series at all levels
  ally <- aggts(object)
  nbottom <- NCOL(object$bts)
  nseries <- NCOL(ally)
  ntime <- NROW(ally)
  # Do Box-Cox transformation if required
  if (!is.null(lambda)) {
    ally <- BoxCox(ally, lambda)
  }
  # Set up matrices to store residuals
  innov_res <- response_res <- matrix(0, nrow=nrow(ally), ncol=nseries)
  models <- list()
  # Loop over all time series and compute forecast means and variances
  for (i in seq(nseries)) {
    # Add some tiny positive noise to avoid zero series
    models[[i]] <- model_function(ally[, i] + abs(rnorm(n=ntime, sd=1e-2)))
    # Grab the residuals
    innov_res[,i] <- residuals(models[[i]], type="innovation")
    response_res[,i] <- residuals(models[[i]], type="response")
  }
  # S matrix
  S <- smatrix(object)
  # Find W matrix
  if (method == "ols") {
    W <- diag(nseries)
  } else {
    W <- hts:::lowerD(response_res)
    if (method == "mint") {
      W <- hts:::shrink.estim(response_res, W)[[1]]
    }
  }
  # Find G matrix
  if (method == "bu") {
    G <- cbind(matrix(0, nrow = nbottom, ncol = nseries - nbottom), diag(nrow = nbottom))
  } else {
    Winv <- solve(W)
    G <- solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv
  }
  SG <- S %*% G

  # Compute prediction intervals using nonparametric method
  sim <- array(0, c(nseries, h, nsim))
  for (j in seq(nsim)) {
    bootres <- innov_res[sample(ntime, size=h, replace=TRUE),]
    for (i in seq(nseries)) {
      sim[i, , j] <- simulate(models[[i]], innov = bootres[,i])
    }
  }
  # Backtransform
  if (!is.null(lambda)) {
    sim <- InvBoxCox(sim, lambda)
  }
  # Project simulated sample paths to coherent space
  for (j in seq(nsim)) {
    sim[, , j] <- SG %*% sim[, , j]
  }
  # Compute reconciled means, variances and quantiles
  #fmean <- t(apply(sim, c(1,2), mean))
  #fvar <- t(apply(sim, c(1,2), var))
  #flower <- t(apply(sim, c(1, 2), quantile, prob = 0.5 - level / 200))
  #fupper <- t(apply(sim, c(1, 2), quantile, prob = 0.5 + level / 200))

  # Return mean, variance and lower and upper limits of prediction interval
  #list(mean = ytilde, var = fvar, lower = flower, upper = fupper)
  
  # Return simulated reconciled sample paths
  dimnames(sim)[[1]] <- colnames(ally)
  dimnames(sim)[[2]] <- paste0("h=",seq(h))
  dimnames(sim)[[3]] <- paste0("sim",seq(nsim))
  return(sim)
}

# Look at RMSSE over several levels for MinT
rmsse <- function(fmean, test_gts, train_gts) {
  alltest <- aggts(test_gts)
  alltrain <- aggts(train_gts)
  scale_factor <- colMeans(diff(alltrain, 7)^2)
  rmsse <- colMeans(sweep((fmean - alltest)^2, 2L, scale_factor, '/'))
  rmsse[scale_factor < .Machine$double.eps] <- 0
  return(rmsse)
}
}
