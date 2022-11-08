library(tidyverse)
library(hts)
library(readr)
library(furrr)
source(here::here("rscript/hts/htsplus.R"))
source(here::here("rscript/hts/glm.R"))
source(here::here("rscript/hts/naive.R"))

# Parallelization
# plan(multisession, workers = 3)

# Read hierarchical/grouped time series
# incident_gts <- read_rds(paste0(storage_folder, "incidents_test_gts.rds"))
incident_gts <- read_rds(paste0(storage_folder, "incidents_gts.rds"))
holidays <- read_rds(paste0(storage_folder, "holidays_ts.rds"))

# Test sets of size 84,
origins <- 42 * seq(10) + 42
for (i in seq(origins)) {
  # Set up training set
  train <- incident_gts
  train$bts <- subset(train$bts, end = nrow(incident_gts$bts) - origins[i])
  # Create reconciled sample paths for different models
  #reconcile_sample_paths(train, model_function = "ets")
  # reconcile_sample_paths(train, model_function = "tscount")
  #reconcile_sample_paths(train, model_function = "iglm")
  reconcile_sample_paths(train, model_function = "naiveecdf")
}

mse <- compute_mse(incident_gts)
mse |> 
  group_by(method, model, series) |> 
  summarise(mse = mean(mse)) |> 
  arrange(mse) |>
  print(n=200)
