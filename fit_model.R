#Resource
#https://cran.r-project.org/web/views/TimeSeries.html
#check: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
#tscount: https://cran.r-project.org/web/packages/tscount/vignettes/tsglm.pdf
#https://pipiras.sites.oasis.unc.edu/timeseries/Nonlinear_2_-_Count_time_series_-_Menu.html
#https://hidda.github.io/forecasting/articles/extra/CHILI_tscount.html
#https://www.youtube.com/watch?v=6mIUmAUj0I0
#https://masalmon.eu/2017/02/12/wikideaths1_ts/
#file:///Users/bahmanrostami-tabar/Downloads/2015_31_report.pdf

library(tscount)
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(MASS)
library(fable.tscount)
library(patchwork)
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
  summarise(incidents = sum(total_incidents)) %>% ungroup() %>% 
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0) %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
                      nature_of_incident = factor(nature_of_incident),
                      lhb_code = factor(lhb_code))


incidents %>% as_tibble() %>% 
  add_count(nature_of_incident,category,lhb_code,sort = TRUE) %>% 
  tail(n=300) %>% View()

incidents_greather_than100 <- incidents %>% as_tibble() %>% 
  add_count(nature_of_incident,category,lhb_code,sort = TRUE) %>% 
  filter(n>100) %>% dplyr::select(-n) %>% as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident))

incidents_gts <- incidents_greather_than100 %>%
  aggregate_key(nature_of_incident * category * lhb_code, incidents = sum(incidents))


f_horizon <- 5*7

train <- incidents_gts %>% filter(date < max(date) - f_horizon)

fit_incident <- train %>%
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS = ETS(sqrt(incidents)),
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend()+season("week") + fourier("year", 10), link = "log", model=list(past_obs=1:4)),
  ) %>%
  reconcile(
    bu_NAIVE = bottom_up(NAIVE),
    mint_NAIVE = min_trace(NAIVE, method = "mint_shrink"),
    bu_ETS = bottom_up(ETS),
    mint_ETS = min_trace(ETS, method = "mint_shrink"),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    mint_TSCOUNT = min_trace(TSCOUNT, method = "mint_shrink")
  )

#write_rds(fit_incident,"fit_incident.rds")

fcst_incident <- fit_incident %>% forecast(h = f_horizon)

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
fcst_incident %>% 
  filter(!is_aggregated(category), 
         !is_aggregated(nature_of_incident), 
         !is_aggregated(lhb_code)) %>%
  accuracy(data = incidents_gts,
           measures = list(mase = MASE,
                           rmsse = RMSSE,
                           crps = CRPS)
  ) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), 
            rmsse = mean(rmsse),
            crps = mean(crps))