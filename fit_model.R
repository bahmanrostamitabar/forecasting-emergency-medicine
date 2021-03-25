#Resource
#https://cran.r-project.org/web/views/TimeSeries.html
#check: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
#tscount: https://cran.r-project.org/web/packages/tscount/vignettes/tsglm.pdf
#https://pipiras.sites.oasis.unc.edu/timeseries/Nonlinear_2_-_Count_time_series_-_Menu.html
#https://hidda.github.io/forecasting/articles/extra/CHILI_tscount.html
#https://www.youtube.com/watch?v=6mIUmAUj0I0
#https://masalmon.eu/2017/02/12/wikideaths1_ts/
#file:///Users/bahmanrostami-tabar/Downloads/2015_31_report.pdf

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)
library(fable.tscount)
library(tscount)

#library(future)
# Read data
incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 
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
 
  
incidents_tsbl <- incidents %>%  
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0, .full = TRUE) %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
                      nature_of_incident = factor(nature_of_incident),
                      lhb_code = factor(lhb_code))

incidents_gts <- incidents_tsbl %>%
   aggregate_key(nature_of_incident * category * lhb_code, incidents = sum(incidents))

f_horizon <- 5*7

incidents_gts_sample <- incidents_gts %>% 
  filter((category == "RED" | category == "GREEN") & (lhb_code == "AB" | lhb_code == "SB") & (nature_of_incident == "BREATHING PROBLEMS" | nature_of_incident == "CHEST PAIN"))

  
  
train <- incidents_gts %>% filter_index("2018-06-30" ~ .) %>% 
  filter(date < max(date) - f_horizon)
train <- incidents_gts_sample %>% filter(date < max(date) - f_horizon)
#plan(multisession)

fit_incident <- train %>%
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS = ETS(sqrt(incidents)),
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend() + season("week") + fourier("year", 10), link = "log", model=list(past_obs=1:4))
  ) 
#fit_incident <- read_rds("fit_incident.rds")
fit_reconcile <- fit_incident %>%
  reconcile(
    bu_ETS = bottom_up(ETS),
    mint_ETS = min_trace(ETS, method = "mint_shrink"),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    mint_TSCOUNT = min_trace(TSCOUNT, method = "mint_shrink")
  )

#fit_reconcile <- read_rds("fit_incident_reconcile.rds")
  
fcst_incident <- fit_reconcile %>% forecast(h = f_horizon)

# Top level
fcst_incident %>% 
  filter(is_aggregated(nature_of_incident),
         is_aggregated(category),
         is_aggregated(lhb_code)) %>%
  accuracy(data = incidents_gts,
           measures = list(mase = MASE,
                           rmsse = RMSSE,
                           crps = CRPS)
           ) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), 
            rmsse = mean(rmsse),
            crps = mean(crps))
  

# Bottom level
acc <- fcst_incident %>% 
  filter(!is_aggregated(category), 
         !is_aggregated(nature_of_incident), 
         !is_aggregated(lhb_code)) %>%
  filter(nature_of_incident!="DIABETIC PROBLEMS", category!="RED",lhb_code!="POW") %>% 
  accuracy(data = incidents_gts,
           measures = list(mase = MASE,
                           rmsse = RMSSE,
                           crps = CRPS,
                           mae = MAE,
                           rmse = RMSE)
  ) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), 
            rmsse = mean(rmsse),
            mae = mean(mae),
            rmse = mean(rmse),
            crps = mean(crps))