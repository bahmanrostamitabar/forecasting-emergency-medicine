library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)
library(fable.tscount)
library(tscount)

f_horizon <- 5*7
# Read data
incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 
# nature of incidents with low volume
nature_of_incident_low <- c("AUTOMATIC CRASH NOTIFICATION",
                            "INACCESSIBLE INCIDENT/OTHER ENTRAP",
                            "INTERFACILITY EVALUATION/TRANSFER",
                            "MAJOR INCIDENT - OVERRIDE PROQA",
                            "TRANSFER/INTERFACILITY/PALLIATIVE")

# read holiday data
holiday <- readxl::read_excel("data/holiday_rugby.xlsx") %>% 
  select(date,public_holiday,school_holiday) %>% 
  mutate(date=as_date(date)) %>% 
  mutate(public_holiday_d=if_else(is.na(public_holiday), 0,1),
         school_holiday_d=if_else(is.na(school_holiday), 0,1)) %>% 
  mutate(xmas=if_else(public_holiday=="Christmas Day","1","0"),
         new_years_day=if_else(public_holiday=="New Years Day",1,0)) %>% 
  mutate_at(vars(xmas,new_years_day), ~replace(.,is.na(.),0)) 

# create dummies for holidays
holiday_dummy <- holiday %>% select(date,
                                    public_holiday_d,
                                    school_holiday_d,
                                    xmas,
                                    new_years_day) %>% 
  mutate_at(vars(public_holiday_d,school_holiday_d,xmas,new_years_day),
            ~as_factor(.)) %>% 
  as_tsibble(index = date)
holiday_school <- holiday_dummy %>% select(date,school_holiday_d) %>%
  filter(school_holiday_d==1) %>% 
  mutate(holiday = "school_holiday") %>% select(-school_holiday_d)
holiday_public <- holiday_dummy %>% select(date,public_holiday_d) %>%
  filter(public_holiday_d==1) %>% 
  mutate(holiday = "public_holiday") %>% select(-public_holiday_d)

holiday_xmas <- holiday_dummy %>% select(date,xmas) %>%
  filter(xmas==1) %>% 
  mutate(holiday = "xmas") %>% select(-xmas)

holiday_newyear <- holiday_dummy %>% select(date,new_years_day) %>%
  filter(new_years_day==1) %>% 
  mutate(holiday = "new_years_day") %>% select(-new_years_day)

# count number of incidents
incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>% 
  mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "upgrade")) %>% 
  mutate(nature_of_incident= ifelse(nature_of_incident %in% nature_of_incident_low,"other" , nature_of_incident)) %>% 
  group_by(lhb_code, category, nature_of_incident, date) %>% 
  summarise(incidents = sum(total_incidents),.groups = "drop") %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
         nature_of_incident = factor(nature_of_incident),
         lhb_code = factor(lhb_code))

incidents_tsbl <- incidents %>%  
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0, .full = TRUE)
write_rds(incidents_tsbl,"data/incidents_tsbl.rds")

incidents_gts <- incidents_tsbl %>%
  aggregate_key(nature_of_incident * category * lhb_code, incidents = sum(incidents))

incidents_gts_holidays <- incidents_gts %>%
  left_join(holiday_dummy, by="date")

# time series cross validation
n_init <- length(incidents_gts_holidays %>% filter_index(. ~ "2018-07-30") %>% pull(date) %>% unique())
train_tscv <- incidents_gts_holidays %>% 
  filter_index(. ~ "2019-06-27") %>% 
  stretch_tsibble(.init = n_init, .step = 6)

# this is to create the test set and assign it to new_data in forecast
test_tscv <- incidents_gts_holidays %>% filter_index("2018-07-31" ~ .) %>%
  slide_tsibble(.size = f_horizon, .step = 6)

# Specify estimating models using multiple core
library(future)
plan(multicore)
fit_incident <- train_tscv %>% 
  model(
    NAIVE = NAIVE(sqrt(incidents)),
    ETS= ETS(sqrt(incidents)),
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend() + season("week") + fourier("year", 10)+public_holiday_d+school_holiday_d+xmas+new_years_day, link = "log", model=list(past_obs=1:13))
  )

fit_reconcile <- fit_incident %>%
  reconcile(
    td_TSCOUNT = top_down(TSCOUNT),
    bu_TSCOUNT = bottom_up(TSCOUNT),
    wls_TSCOUNT = min_trace(TSCOUNT, method = "wls_struct"),
    td_ETS = top_down(ETS),
    bu_ETS = bottom_up(ETS),
    wls_ETS = min_trace(ETS, method = "wls_struct")
  )

fcst_incident <- fit_reconcile %>% 
  forecast(new_data=test_tscv)

# Look at MASE over several levels for MinT
fcst_accuracy <- fcst_incident %>%
  accuracy(incidents_gts,
           measures = list(rmsse = RMSSE, mase = MASE)
  ) 

# Bottom level
fcst_accuracy %>% 
  filter(!is_aggregated(lhb_code), !is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps=mean(crps), winkler=mean(winkler))

# At LHB level
fcst_accuracy %>% 
  filter(is_aggregated(lhb_code), !is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps=mean(crps), winkler=mean(winkler))
# At category level
fcst_accuracy %>% 
  filter(!is_aggregated(lhb_code), is_aggregated(category), !is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps=mean(crps), winkler=mean(winkler))
# Top level
fcst_accuracy %>% 
  filter(is_aggregated(lhb_code), is_aggregated(category), is_aggregated(nature_of_incident)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase), crps=mean(crps), winkler=mean(winkler))