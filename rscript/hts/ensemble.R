library(stringr)
library(readr)

create_ensembles <- function(models_to_use = "all") {
  # Find simulation files
  files <- fs::dir_ls(storage_folder, glob = paste0("*_sim_*.rds"))
  # Find models
  models <- str_remove(files, storage_folder) |> 
    str_extract("[a-zA-Z]*_") |> 
    str_remove("_") |> 
    unique()
  if(!identical(models_to_use, "all")) {
    models <- models[models %in% models_to_use]
    if(!identical(models, models_to_use)) {
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
  for(j in seq_along(methods)) {
    for(k in seq_along(origins)) {
     create_specific_ensemble(models_to_use, methods[j], origins[k])
    }
  }
  
}

create_specific_ensemble <- function(models, method, origin) {
  # Form file name for saving results
  filename <- paste0(storage_folder,"ensemble_",origin,"_sim_",method,".rds")
  # Check if this has already been run
  if (fs::file_exists(filename)) {
    # Just use the previous results
    return(invisible(read_rds(filename)))
  }

  # Find simulation files
  files <- fs::dir_ls(storage_folder, glob = paste0("*_",origin,"_sim_",method,".rds"))
  # For each training set, combine available simulation files
  new_sim <- array(0, c(1224, 84, 1000*length(files_i)))
  for(j in seq_along(files_i)) {
    new_sim[,,1000*(j-1) + seq(1000)] <- read_rds(files_i[j])
  }
  write_rds(new_sim, filename, compress="bz2")
}
    
