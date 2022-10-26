# htsplus = Use forecast reconciliation to generate simulated future sample paths

# object = gts object
# h = forecast horizon
# model_function = function used to model each time series. e.g., ets or auto.arima or tscount
# methods = vector of reconcilation methods. All will be used
# nsim = number of simulations in computing prediction intervals using nonparametric method

htsplus <- function(object,
                    h,
                    model_function = auto.arima,
                    methods,
                    nsim = 50) {
  # Generate all time series at all levels
  ally <- aggts(object)
  # Loop over all time series and store models
  fit_model <- function(y, model_function) {
    ntime <- length(y)
    if (forecast:::is.constant(y)) {
      # Series is constant. So just fit a mean zero model with small variance
      return(Arima(y + rnorm(ntime, sd = 1e-3), order = c(0, 0, 0), include.mean = FALSE))
    } else {
      return(model_function(y))
    }
  }
  models <- furrr::future_map(as.list(ally), fit_model,
    model_function = model_function,
    .options = furrr_options(seed = NULL)
  )
  # Grab the residuals from each model
  nseries <- NCOL(ally)
  ntime <- NROW(ally)
  innov_res <- response_res <- matrix(0, nrow = ntime, ncol = nseries)
  for (i in seq(nseries)) {
    response_res[, i] <- residuals(models[[i]], type = "response")
    if (!identical(model_function, tscount)) {
      innov_res[, i] <- residuals(models[[i]], type = "innovation")
    }
  }
  # S matrix
  S <- smatrix(object)
  # Find W matrices
  W1 <- diag(nseries)
  W2 <- hts:::lowerD(response_res)
  W3 <- hts:::shrink.estim(response_res, W2)[[1]]

  # Find G matrix
  nbottom <- NCOL(object$bts)
  G1 <- cbind(matrix(0, nrow = nbottom, ncol = nseries - nbottom), diag(nrow = nbottom))
  Winv <- solve(W2)
  G2 <- solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv
  Winv <- solve(W3)
  G3 <- solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv
  
  # Computing mapping matrix M
  M1 <- S %*% G1
  M2 <- S %*% G2
  M3 <- S %*% G3
  
  # Compute reconciled future sample paths using cross-sectional bootstrap
  nmethods <- length(methods)
  sim <- array(0, c(nseries, h, nsim, nmethods))
  dimnames(sim)[[1]] <- colnames(ally)
  dimnames(sim)[[2]] <- paste0("h=", seq(h))
  dimnames(sim)[[3]] <- paste0("sim", seq(nsim))
  dimnames(sim)[[4]] <- methods
  for (j in seq(nsim)) {
    bootres <- innov_res[sample(ntime, size = h, replace = TRUE), ]
    for (i in seq(nseries)) {
      sim[i, , j, 1] <- simulate(models[[i]], innov = bootres[, i])
      # Use same sample paths for other reconciliation methods
      for(k in seq(nmethods)[-1])
        sim[i, , j, k] <- sim[i, , j, 1]
    }
    sim[, , j, 1] <- M1 %*% sim[, , j, 1]
    sim[, , j, 2] <- M2 %*% sim[, , j, 2]
    sim[, , j, 3] <- M3 %*% sim[, , j, 3]
  }
  # Return simulated reconciled sample paths
  return(sim)
}

# Compute RMSSE given forecast mean and training/test gts objects
rmsse <- function(sim, test_gts, train_gts) {
  fmean <- apply(sim, c(1, 2, 4), mean)
  alltest <- aggts(test_gts)
  alltrain <- aggts(train_gts)
  scale_factor <- colMeans(diff(alltrain, 7)^2)
  rmsse <- fmean[,1,]
  for(i in NCOL(rmsse)) {
    rmsse[,i] <- colMeans(sweep((t(fmean[,,i]) - alltest)^2, 2L, scale_factor, "/"))
  }
  # Set RMSSE of zero series to zero
  rmsse[scale_factor < .Machine$double.eps] <- 0
  return(t(rmsse))
}

# Compute CRPS given simulated values x and actual y
crps_sample <- function(x, y) {
  # Set CRPS of zero series to zero
  if (var(x) < 1e-5) {
    return(0)
  }
  x <- sort(x)
  m <- length(x)
  crps <- (2 / m) * mean((x - y) * (m * (y < x) - seq_len(m) + 0.5))
}

# Compute CRPS given simulated sample paths and test gts object
crps <- function(sim, test_gts) {
  alltest <- aggts(test_gts)
  nseries <- dim(sim)[[1]]
  H <- dim(sim)[[2]]
  nmethods <- dim(sim)[[4]]
  crps <- array(0,c(nseries, H, nmethods))
  dimnames(crps) <- dimnames(sim)[c(1,2,4)]
  for (i in seq(nseries)) {
    for (h in seq(H)) {
      for(m in seq(nmethods)) {
        crps[i, h, m] <- crps_sample(sim[i, h,, m], alltest[h, i])
      }
    }
  }
  return(apply(crps, c(3,1), mean))
}
