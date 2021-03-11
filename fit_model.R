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
library(lubridate)
library(MASS)
library(fable.tscount)
library(patchwork)
# Read data
incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 
incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>% 
  mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "other")) %>% 
  group_by(lhb_code, category, nature_of_incident, date) %>% 
  summarise(incidents = sum(total_incidents)) %>% ungroup() %>% 
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0)



# prepare data, look at two different series 
breathing_problem_ts <- incidents %>% index_by(date) %>% 
  group_by(nature_of_incident) %>% 
  summarise(incidents = sum(incidents)) %>% 
  filter(nature_of_incident == "BREATHING PROBLEMS") %>%  
  dplyr::select(-nature_of_incident) %>% ungroup()

# this ts is ntermittent
choking_ts <- incidents %>% index_by(date) %>% 
  group_by(nature_of_incident) %>% 
  summarise(incidents = sum(incidents)) %>% 
  filter(nature_of_incident == "CHOKING") %>%  
  dplyr::select(-nature_of_incident) %>% ungroup()

breathing_problem_tsbl <- breathing_problem_ts %>% as_tsibble()
choking_tsbl <- choking_ts %>% as_tsibble()

p1 <- breathing_problem_tsbl %>% autoplot()
p2 <- choking_tsbl %>% autoplot()
p1/p2

#---split data-------

#data_incident <- breathing_problem_tsbl
data_incident <- choking_tsbl

f_horizon <- 5*7

#without TSCV
fit <- data_incident %>% filter_index(~ "2019-06-26") %>%
  model(
    ETS = ETS(incidents),
    SNAIVE = SNAIVE(incidents),
    TSCOUNT = fable.tscount::TSCOUNT(incidents),
    TSCOUNT_season = fable.tscount::TSCOUNT(incidents ~ season("week")),
  )
# 1.Can we use fourier(), e.g with TSCOUNT? model(fable.tscount::TSCOUNT(incidents ~ fourier("week", 3)+fourier("year", 5)))
#it gives an error as it has negative values
# 2. Where do we  specify the distribution : dist = poisson or dist = nbinom
#3. Do we need to specify the link function required in tsglm
#model(fable.tscount::TSCOUNT(incidents ~ fourier("week", 3)))
#fcst <- fit %>% forecast(times = 5000,  h = f_horizon, bootstrap = TRUE) 
fcst_sim <- fit %>%
  generate(times = 5000, h = f_horizon, bootstrap = TRUE) 
# for low values/intermittent series & SNAIVE and ETS, we get negative values 
#fcst_sim_non_negative <-  fcst_sim %>% mutate(.sim= if_else(.sim <0,0,.sim))

fcst_dist <- fcst_sim %>% 
  group_by(.model) %>% 
  summarise(
    incidents = dist_sample(list(.sim)),
    .mean = mean(incidents)
  ) %>% ungroup()

fsct_fable <- fcst_dist %>% 
  as_fable(distribution = incidents, response = "incidents")

fsct_fable %>% group_by(.model) %>% 
  mutate(h=row_number()) %>% ungroup()->fsct_fable_h

accuracy_total <- fsct_fable_h %>%
  accuracy(data_incident, 
           measures = list(point_accuracy_measures,CRPS = CRPS))
accuracy_total
accuracy_h <- fsct_fable_h %>%
  accuracy(data_incident, 
           measures = list(point_accuracy_measures,CRPS = CRPS), 
           by = c(".model","h"))
accuracy_h %>% dplyr::select(.model,h,CRPS) %>% 
  ggplot(aes(x=h, y=CRPS, colour = .model))+
  geom_point()+
  geom_line()

#forecast with TSCV

#timeseires cross validation
n_init <- nrow(data_incident %>% filter_index(~ "2018-12-31")) 
ae_tscv <- data_incident %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = n_init, .step = 7 )
ae_test <- data_incident %>% 
  filter_index("2019-01-01" ~ .) %>% 
  slide_tsibble(.size = f_horizon, .step = 7)

fit <- ae_tscv %>%
  model(
    ETS = ETS(incidents),
    SNAIVE = SNAIVE(incidents),
    TSCOUNT = fable.tscount::TSCOUNT(incidents),
    TSCOUNT_season = fable.tscount::TSCOUNT(incidents ~ season("week"))
  )

fcst_sim <- fit %>%
  generate(times = 5000, h = f_horizon, bootstrap = TRUE) 

fcst_sim_non_negative <-  fcst_sim %>% mutate(.sim= if_else(.sim <0,0,.sim))

fcst_dist <- fcst_sim_non_negative %>% 
  group_by(.model,.id) %>% 
  summarise(
    incidents = dist_sample(list(.sim)),
    .mean = mean(incidents)
  ) %>% ungroup()

fsct_fable <- fcst_dist %>% 
  as_fable(distribution = incidents, response = "incidents")

fsct_fable %>% group_by(.id,.model) %>% 
  mutate(h=row_number()) %>% ungroup()->fsct_fable_h

accuracy_total <- fsct_fable_h %>%
  accuracy(data_incident, 
           measures = list(point_accuracy_measures,CRPS = CRPS))
accuracy_total
accuracy_h <- fsct_fable_h %>%
  accuracy(data_incident, 
           measures = list(point_accuracy_measures,CRPS = CRPS), 
           by = c(".model","h"))
accuracy_h %>% dplyr::select(.model,h,CRPS) %>% 
  ggplot(aes(x=h, y=CRPS, colour = .model))+
  geom_point()+
  geom_line()