library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)
library(fable.tscount)
#library(tscount)

# Read data
incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 
# prepare data
nature_of_incident_low <- c("AUTOMATIC CRASH NOTIFICATION",
                            "INACCESSIBLE INCIDENT/OTHER ENTRAP",
                            "INTERFACILITY EVALUATION/TRANSFER",
                            "MAJOR INCIDENT - OVERRIDE PROQA",
                            "TRANSFER/INTERFACILITY/PALLIATIVE")
incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>% 
  mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "upgrade")) %>% 
  mutate(nature_of_incident= ifelse(nature_of_incident %in% nature_of_incident_low,"other" , nature_of_incident)) %>% 
  group_by(lhb_code, category, nature_of_incident, date) %>% 
  summarise(incidents = sum(total_incidents),.groups = "drop") 

# create tsibble
incidents_tsbl <- incidents %>%  
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0, .full = TRUE) %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
         nature_of_incident = factor(nature_of_incident),
         lhb_code = factor(lhb_code))

# create grouped structure
incidents_gts <- incidents_tsbl %>%
  aggregate_key(nature_of_incident * category * lhb_code, incidents = sum(incidents))
#minimal example
incidents_gts_sample <- incidents_gts %>% 
  filter((category == "RED" | category == "GREEN") & (lhb_code == "AB" | lhb_code == "SB") & (nature_of_incident == "BREATHING PROBLEMS" | nature_of_incident == "CHEST PAIN"))
f_horizon <- 5*7
train <- incidents_gts_sample %>% filter(date < max(date) - f_horizon)

fit_incident <- train %>%
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS = ETS(sqrt(incidents)),
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend() + season("week") + fourier("year", 10), link = "log", model=list(past_obs=1:4))
  ) %>%
  reconcile(
    bu_ETS = bottom_up(ETS),
    mint_ETS = min_trace(ETS, method = "mint_shrink"),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    mint_TSCOUNT = min_trace(TSCOUNT, method = "mint_shrink")
  )

fcst_incident <- fit_incident %>% forecast(h = f_horizon)
