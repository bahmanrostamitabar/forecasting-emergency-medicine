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
    if(forecast:::is.constant(ally[,i])) {
      # Series is constant. So just fit a mean zero model with small variance
      models[[i]] <- Arima(ally[,i] + rnorm(ntime, sd=1e-3), order=c(0,0,0), include.mean = FALSE)
    } else {
      models[[i]] <- model_function(ally[, i])
    }
    # Grab the residuals
    response_res[,i] <- residuals(models[[i]], type="response")
    if(!identical(model_function, tscount)) {
      innov_res[,i] <- residuals(models[[i]], type="innovation")
    } 
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

# Compute RMSSE given forecast mean and training/test gts objects
rmsse <- function(sim, test_gts, train_gts) {
  fmean <- t(apply(sim, c(1,2), mean))
  alltest <- aggts(test_gts)
  alltrain <- aggts(train_gts)
  scale_factor <- colMeans(diff(alltrain, 7)^2)
  rmsse <- colMeans(sweep((fmean - alltest)^2, 2L, scale_factor, '/'))
  # Set RMSSE of zero series to zero
  rmsse[scale_factor < .Machine$double.eps] <- 0
  return(rmsse)
}

# Compute CRPS given simulated values x and actual y
crps_sample <- function(x, y) {
  # Set CRPS of zero series to zero
  if(var(x) < 1e-5)
    return(0)
  x <- sort(x)
  m <- length(x)
  crps <- (2/m) * mean((x - y) * (m * (y < x) - seq_len(m) +  0.5))
}

# Compute CRPS given simulated sample paths and test gts object
crps <- function(sim, test_gts) {
  alltest <- aggts(test_gts)
  nseries <- dim(sim)[[1]]
  H <- dim(sim)[[2]]
  crps <- matrix(0, nrow=H, ncol=nseries)
  colnames(crps) <- dimnames(sim)[[1]]
  rownames(crps) <- dimnames(sim)[[2]]
  for(i in seq(nseries)) {
    for(h in seq(H)) {
      crps[h, i] <- crps_sample(sim[i,h,], alltest[h,i])
    }
  }
  return(colMeans(crps))
}

