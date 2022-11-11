# Compute MSE for all models and methods
# train = gts object training data

compute_accuracy <- function(train, measure = "mse") {
  # Has this already been run?
  filename <- paste0(storage_folder, measure, ".rds")
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    return(invisible(read_rds(filename)))
  }

  # Find simulation files
  files <- fs::dir_ls(storage_folder, glob = paste0("*_sim_*.rds"))
  # Find models
  models <- str_remove(files, storage_folder) |>
    str_extract("[a-zA-Z]*_") |>
    str_remove("_") |>
    unique()
  # Find methods
  methods <- str_remove(files, storage_folder) |>
    str_extract("[a-zA-Z]*.rds") |>
    str_remove(".rds") |>
    unique()
  methods <- methods[methods != ""]
  accuracy <- NULL
  for (i in seq_along(models)) {
    for (j in seq_along(methods)) {
      accuracy_tmp <- compute_accuracy_specific(train, models[i], methods[j], measure)
      accuracy <- bind_rows(
        accuracy,
        tibble(
          method = methods[j], model = models[i],
          h = rep(1:84, rep(5, 84)), accuracy = c(accuracy_tmp),
          series = rep(c("Overall", "Total", "Control areas", "Health boards", "Bottom"), 84)
        )
      )
    }
  }
  colnames(accuracy)[4] <- measure
  write_rds(accuracy, filename, compress = "bz2")
  return(accuracy)
}

# Compute MSE for a specific model and reconciliation method
# train = gts object training data
# model_function = function used to model each time series. e.g., ets or auto.arima or tscount
# method = method of reconciliation

compute_accuracy_specific <- function(train, model_function = "ets", method = "wls",
                                      measure = c("mse", "rmsse", "mase", "crps")) {
  measure <- match.arg(measure)
  # Find simulation files
  files <- fs::dir_ls(storage_folder, glob = paste0("*", model_function, "_*_sim_", method, ".rds"))
  # Dimensions
  norigins <- length(files)
  nb <- NCOL(train$bts)
  alltrain <- aggts(train)
  nseries <- NCOL(alltrain)
  alltrain <- t(alltrain)
  e <- array(0, c(nseries, 84, norigins))
  for (i in seq(norigins)) {
    sim <- read_rds(files[i])
    sim <- apply(sim, c(1, 2), mean)
    n <- parse_number(files[i])
    e[, , i] <- sim - alltrain[, n + seq(84)]
  }
  if (measure == "mse") {
    accuracy <- apply(e^2, c(1, 2), mean)
  } else if (measure == "rmsse") {
    # Use insample MSE as scaling factor
    scale_factor <- rowMeans(sweep(alltrain, 1, rowMeans(alltrain))^2)
    mse <- apply(e^2, c(1, 2), mean)
    accuracy <- sweep(mse, 1, scale_factor, FUN = "/")
  } else if (measure == "mase") {
    # Use insample MAE as scaling factor
    scale_factor <- rowMeans(abs(sweep(alltrain, 1, rowMeans(alltrain))))
    mae <- apply(abs(e), c(1, 2), mean)
    accuracy <- sweep(mae, 1, scale_factor, FUN = "/")
  } else if (measure == "crps") {
    accuracy <- matrix(0, nrow = nseries, ncol = 84)
    for (j in seq(nseries)) {
      for (k in seq(84)) {
        accuracy[j, k] <- crps_sample(e[j, k, ], 0)
      }
    }
    stop("not yet implemented")
  }

  # Overall accuracy
  overall <- colMeans(accuracy)
  # Collapse bottom level
  bottom <- colMeans(tail(accuracy, nb))
  # Collapse control areas
  control_areas <- colMeans(accuracy[2:4, ])
  # Collapse health boards
  health_boards <- colMeans(accuracy[5:11, ])
  # Combine
  accuracy <- rbind(overall, accuracy[1, ], control_areas, health_boards, bottom)
  colnames(accuracy) <- colnames(sim)
  rownames(accuracy) <- c("Overall", "Total", "Control areas", "Health boards", "Bottom")
  if (measure == "rmsse") {
    return(sqrt(accuracy))
  } else {
    return(accuracy)
  }
}

# Compute CRPS given simulated values x and actual y
crps_sample <- function(x, y) {
  # Set CRPS of zero series to zero
  if (var(x) < 1e-5) {
    return(0)
  }
  x <- sort(x)
  m <- length(x)
  return((2 / m) * mean((x - y) * (m * (y < x) - seq_len(m) + 0.5)))
}
