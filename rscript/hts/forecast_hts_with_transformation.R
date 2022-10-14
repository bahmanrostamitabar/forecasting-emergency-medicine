library(hts)
library(readr)
source("htsplus.R")

# Test on incident data
incident_all <- read_rds("data/incident_all.rds")
incident_ts <- ts(incident_all, frequency = 7)

# create hierarchical/grouped time series
gts_incident <- hts::gts(incident_ts, characters = list(c(1, 2), 3, 9))
fcst_bu <- htsplus(gts_incident, h = 10, model_function = ets, lambda = 0.5, method = "bu")
fcst_wls <- htsplus(gts_incident, h = 10, model_function = ets, lambda = 0.5, method = "wls")
