# fit_models: Function to fit models to incidents data and save results as a list

# object = gts object
# model_function = function used to model each time series.
# e.g., ets or auto.arima or tscount

fit_models <- function(object, model_function = "ets") {
  # Number of observations in training set
  ntime <- NROW(object$bts)

  # Form file name for saving results
  filename <- here::here(paste0("rscript/hts/", model_function, "_", ntime, ".rds"))
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    # Just use the previous results
    return(invisible(read_rds(filename)))
  }

  # Generate all time series at all levels
  ally <- aggts(object)
  # Loop over all time series and store models
  fit_model <- function(y, model_function) {
    ntime <- length(y)
    modelfn <- get(model_function)
    if (forecast:::is.constant(y)) {
      # Series is constant. So just fit a mean zero model with small variance
      return(Arima(y + rnorm(ntime, sd = 1e-3), order = c(0, 0, 0), include.mean = FALSE))
    } else {
      return(modelfn(y))
    }
  }
  models <- furrr::future_map(as.list(ally), fit_model,
    model_function = model_function,
    .options = furrr_options(seed = NULL)
  )
  # Save results to file and then return them
  write_rds(models, filename)
  return(invisible(models))
}

# make_mapping_matrices: Function to create mapping matrices for incident data
# and save results as a list

# object = gts object
# model_function = output from fit_models()
# type = Type of residual to store

calculate_residuals <- function(object, model_function, type=c("innovation","response")) {
  # Type of residual to store
  type <- match.arg(type)
  
  ntime <- NROW(object$bts)
  
  # Form file name for saving results
  filename <- here::here(paste0("rscript/hts/", model_function, "_", ntime, "_res_",type,".rds"))
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    return(read_rds(filename))
  }
  
  # Grab models
  models <- fit_models(object, model_function)
  ntime <- NROW(object$bts)
  nseries <- length(models)
  
  # Compute the residuals from each model
  res <- matrix(0, nrow = ntime, ncol = nseries)
  for (i in seq(nseries)) {
    res[, i] <- residuals(models[[i]], type = type)
  }
  
  # Save results to file and then return them
  write_rds(res, filename)
  return(invisible(res))
}


# make_mapping_matrices: Function to create mapping matrices for incident data
# and save results as a list

# object = gts object
# model_function = function used to model each time series.

make_mapping_matrices <- function(object, model_function) {
  ntime <- NROW(object$bts)
  
  # Form file name for saving results
  filename <- here::here(paste0("rscript/hts/", model_function, "_", ntime, "_mapping.rds"))
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    # Just use the previous results
    return(invisible(read_rds(filename)))
  }

  # Grab the residuals from each model
  response_res <- calculate_residuals(object, model_function, type = "response")
  nseries <- NCOL(response_res)
  
  # Create S matrix
  S <- smatrix(object)

  # Find W matrices
  W1 <- diag(nseries)
  W2 <- hts:::lowerD(response_res)
  W3 <- hts:::shrink.estim(response_res, W2)[[1]]

  # Find G matrix
  nbottom <- NCOL(object$bts)
  G1 <- cbind(matrix(0, nrow = nbottom, ncol = nseries - nbottom), diag(nrow = nbottom))
  Winv <- MASS::ginv(W2)
  G2 <- solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv
  Winv <- MASS::ginv(W3)
  G3 <- solve(t(S) %*% Winv %*% S) %*% t(S) %*% Winv

  # Computing mapping matrix M
  M1 <- S %*% G1
  M2 <- S %*% G2
  M3 <- S %*% G3

  # Return list of matrices
  mapping_matrices <- list(M1 = M1, M2 = M2, M3 = M3)

  # Save results to file and then return them
  write_rds(mapping_matrices, filename)
  return(invisible(mapping_matrices))
}

# future_sample_paths = Generate simulated future sample paths for each model

# object = gts object
# h = forecast horizon
# model_function = function used to model each time series. e.g., ets or auto.arima or tscount
# nsim = number of simulations in computing prediction intervals using nonparametric method

future_sample_paths <- function(object, model_function = "ets", h = 84, nsim = 1000) {
  ntime <- NROW(object$bts)

  # Form file name for saving results
  filename <- here::here(paste0("rscript/hts/", model_function, "_", ntime, "_sim.rds"))
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    # Just use the previous results
    return(invisible(read_rds(filename)))
  }
  
  models <- fit_models(object, model_function)
  restype <- dplyr::if_else(identical(model_function,"tscount"), "response", "innovation")
  innov_res <- calculate_residuals(object, model_function, type = restype)
  nseries <- length(models)

  # Compute reconciled future sample paths using cross-sectional bootstrap
  sim <- array(0, c(nseries, h, nsim))
  dimnames(sim)[[1]] <- names(models)
  dimnames(sim)[[2]] <- paste0("h=", seq(h))
  dimnames(sim)[[3]] <- paste0("sim", seq(nsim))
  for (j in seq(nsim)) {
    bootres <- innov_res[sample(ntime, size = h, replace = TRUE), ]
    for (i in seq(nseries)) {
      sim[i, , j] <- simulate(models[[i]], innov = bootres[, i])
    }
  }
  # Set negative to zero
  sim[sim < 0] <- 0
  # Save results to file and then return them
  write_rds(sim, filename)
  return(invisible(sim))
}

# reconcile_sample_paths = Reconcile simulated future sample paths

# object = gts object
# model_function = function used to model each time series. e.g., ets or auto.arima or tscount
# method = method of reconciliation

reconcile_sample_paths <- function(object, model_function = "ets", methods = c("bu", "wls", "mint")) {
  ntime <- NROW(object$bts)
  
  # Has this already been run?
  filename <- here::here(paste0("rscript/hts/", model_function, "_", ntime, 
                                  "_sim_",methods[1],".rds"))
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    return(read_rds(filename))
  }

  # Otherwise reconcile for all the methods, and return the first one
  sim <- future_sample_paths(object, model_function)
  M <- make_mapping_matrices(object, model_function)

  nsim <- dim(sim)[3]
  filestem <- here::here(paste0("rscript/hts/", model_function, "_", ntime, "_sim_"))
  for(k in rev(seq_along(methods))) {
    newsim <- sim
    for(j in seq(nsim)) {
      newsim[,,j] <- M[[k]] %*% sim[,,j]
    }
    # Set negative to zero
    newsim[newsim < 0] <- 0
    write_rds(newsim, paste0(filestem,methods[k],".rds"))
  }
  return(invisible(newsim))
}

# Compute RMSSE given forecast mean and training/test gts objects

# train_gts = gts object training data
# test_gts = gts object test data
# model_function = function used to model each time series. e.g., ets or auto.arima or tscount
# method = method of reconciliation

rmsse <- function(train_gts, test_gts, model_function, method) {
  # Check if this has already been run
  filename <- here::here(paste0("rscript/hts/", model_function, "_", ntime, 
                                "_rmsse_",method,".rds"))
  if (fs::file_exists(filename)) {
    return(read_rds(filename))
  }
  
  # Find the simulation files that exist
  stem <- filename <- here::here(paste0("rscript/hts/", model_function, "_*_sim_", ".rds"))
  
  sim <- reconcile_sample_paths(train_gts, model_function, methods = method)

  # Set up rmsse object
  fmean <- apply(sim, c(2, 1), mean)
  alltest <- aggts(test_gts)
  alltrain <- aggts(train_gts)
  scale_factor <- colMeans(diff(alltrain, 7)^2)
  for (i in NCOL(rmsse)) {
    rmsse[, i] <- colMeans(sweep((t(fmean[, , i]) - alltest)^2, 2L, scale_factor, "/"))
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
  crps <- array(0, c(nseries, H, nmethods))
  dimnames(crps) <- dimnames(sim)[c(1, 2, 4)]
  for (i in seq(nseries)) {
    for (h in seq(H)) {
      for (m in seq(nmethods)) {
        crps[i, h, m] <- crps_sample(sim[i, h, , m], alltest[h, i])
      }
    }
  }
  return(apply(crps, c(3, 1), mean))
}
