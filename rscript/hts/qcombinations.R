library(stringr)
library(readr)

create_qcomb <- function(models_to_use = "all") {
  # Find simulation files
  files <- fs::dir_ls(storage_folder, glob = paste0("*_sim_*.rds"))
  # Find models
  models <- str_remove(files, storage_folder) |>
    str_extract("[a-zA-Z]*_") |>
    str_remove("_") |>
    unique()
  if (!identical(models_to_use, "all")) {
    models <- models[models %in% models_to_use]
    if (!identical(models, models_to_use)) {
      stop("Some models not available")
    }
  }
  # Find methods
  methods <- str_remove(files, storage_folder) |>
    str_extract("[a-zA-Z]*.rds") |>
    str_remove(".rds") |>
    unique()
  methods <- methods[methods != ""]
  # Find separate training sets
  origins <- readr::parse_number(files) |>
    unique() |>
    sort() |>
    as.character()
  for (j in seq_along(methods)) {
    for (k in seq_along(origins)) {
      create_specific_qcomb(models_to_use, methods[j], origins[k])
    }
  }
}

create_specific_qcomb <- function(models, method, origin) {
  # Form file name for saving results
  if (all(models == "all")) {
    filename <- paste0(storage_folder, "qcomb_", origin, "_sim_", method, ".rds")
  } else {
    filename <- paste0(storage_folder, "qcomb2_", origin, "_sim_", method, ".rds")
  }
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    # Just use the previous results
    return(invisible(read_rds(filename)))
  }

  # Find simulation files
  files <- fs::dir_ls(storage_folder, glob = paste0("*_", origin, "_sim_", method, ".rds"))
  files <- files[!grepl("ensemble", files)]
  files <- files[!grepl("qcomb", files)]
  if (all(models != "all")) {
    # Remove naive
    files <- files[!grepl("naive*", files)]
  }

  # For each training set, load available simulation files and compute quantiles
  # with same average quantiles
  prob <- seq(0.001, 0.999, by = 0.001)
  q <- array(0, c(1224, 84, length(files), length(prob)))
  for (k in seq_along(files)) {
    tmp <- readr::read_rds(files[k])
    for (i in seq(1224)) {
      for (j in seq(84)) {
        q[i, j, k, ] <- quantile(tmp[i, j, ], prob = prob)
      }
    }
  }
  aveq <- apply(q, c(1, 2, 4), mean)
  # Generate simulated data with same distribution
  sim <- array(0, c(1224, 84, 1000))
  for (j in seq(1224)) {
    for (k in seq(84)) {
      sim[j, k, ] <- sample(aveq[j, k, ], size = 1000, replace = TRUE)
    }
  }
  write_rds(sim, filename, compress = "bz2")
}
