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
f_horizon <- 7
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
 
# incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>%
#   mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "upgrade")) %>%
#   mutate(nature_of_incident= ifelse(nature_of_incident %in% nature_of_incident_low,"other" , nature_of_incident)) %>%
#   group_by(lhb_code, category, nature_of_incident, date) %>%
#   summarise(incidents = sum(total_incidents)) %>% ungroup() %>%
#   as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
#   fill_gaps(incidents = 0) %>%
#   mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
#          nature_of_incident = factor(nature_of_incident),
#          lhb_code = factor(lhb_code)) %>% as_tibble() %>%
#   add_count(nature_of_incident,category,lhb_code,sort = TRUE) %>%
#   filter(n>1) %>% dplyr::select(-n)

incidents_tsbl <- incidents %>%  
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0, .full = TRUE) %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
                      nature_of_incident = factor(nature_of_incident),
                      lhb_code = factor(lhb_code)) %>% filter(date < (max(date) - f_horizon))

incidents_gts <- incidents_tsbl %>%
   aggregate_key(nature_of_incident * category * lhb_code, incidents = sum(incidents))



#train <- incidents_gts %>% filter(date < (max(date) - f_horizon))

#plan(multisession)

fit_incident <- incidents_gts %>%
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS = ETS(sqrt(incidents)),
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend() + season("week") + fourier("year", 10), link = "log", model=list(past_obs=1:4))
  ) 

fit_reconcile <- fit_incident %>%
  reconcile(
    bu_ETS = bottom_up(ETS),
    mint_ETS = min_trace(ETS, method = "mint_shrink"),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    mint_TSCOUNT = min_trace(TSCOUNT, method = "mint_shrink")
  )
  
fcst_incident <- fit_reconcile %>% forecast(h = f_horizon)

