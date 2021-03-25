library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fable)
library(feasts)
library(ggrepel)

# Read data -------------------------------------------------------------------

temperatures <- fs::dir_ls("data", regex = "midas*") %>%
  purrr::map_dfr(read_csv, skip=90, col_types="cccccccccccccccccccccc") %>%
  filter(ob_end_time != "end data") %>%
  transmute(
    date = as.Date(ob_end_time),
    hour = as.numeric(ob_hour_count),
    min_air_temp = as.numeric(min_air_temp),
    max_air_temp = as.numeric(max_air_temp)
  ) %>%
  group_by(date) %>%
  summarise(
    date = min(date),
    min_air_temp = min(min_air_temp),
    max_air_temp = max(max_air_temp)
  ) %>%
  ungroup()

holiday_rugby <- readxl::read_excel("data/holiday_rugby.xlsx") %>% 
  force_tz(date, tz = "GB") %>% 
  mutate(
    date= as_date(date)
  ) %>% 
  as_tsibble(index = date)


incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 

incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>% 
  mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "other")) %>% 
  group_by(lhb_code, category, nature_of_incident, date) %>% 
  summarise(incidents = sum(total_incidents)) %>% ungroup() %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN"))) %>% 
  left_join(temperatures, by="date") %>%
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0)
 
#count nature of incidents
incidents %>% as_tibble() %>% group_by(nature_of_incident) %>% 
  summarise(incident =sum(incidents)) %>% ungroup() %>% 
  ggplot(aes(x=fct_reorder(nature_of_incident, incident))) +  
  geom_point(aes( y =incident), size =3) +
  coord_flip()+
  labs(x ="Nature of incident")

#Top 15 incidents
incidents %>% as_tibble() %>% group_by(nature_of_incident) %>% 
  summarise(incident =sum(incidents)) %>% ungroup() %>% 
slice_max(n=15, order_by = incident) %>% pull(nature_of_incident)-> selected_ni

# Seasonality in nature_of_incidents

incidents_feature <- incidents %>%
  filter(nature_of_incident %in% selected_ni) %>% 
  index_by(date) %>% 
  group_by(lhb_code,nature_of_incident) %>% summarise(incidents=sum(incidents)) %>% 
  features(incidents, feature_set(pkgs = "feasts")) %>%
  mutate(
    lhb_code = factor(as.character(lhb_code), level=c(sort(unique(incidents$lhb_code)))),
    nature_of_incident = factor(as.character(nature_of_incident), level=c(sort(selected_ni)))
  )

incidents_feature %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_week, col=factor(nature_of_incident))) +
  geom_point()  +
  geom_label_repel(data = incidents_feature %>% filter(seasonal_strength_week > 0.25),
                   aes(label = glue::glue("{lhb_code}/{nature_of_incident}")),
                   show.legend=FALSE)+
  facet_grid(rows = vars(lhb_code))

# day of week effect for top 15 nature of incident
 incidents %>% 
   index_by(date) %>% group_by(nature_of_incident) %>% 
   summarise(incidents=sum(incidents)) %>% 
   filter(nature_of_incident %in% selected_ni) %>% 
   gg_season(period = "week") +
   facet_wrap(vars(nature_of_incident),ncol = 3, scales = "free_y")+
   theme(legend.position = "")

 # monthly seasonality for top 15 nature of incidents 
 incidents %>% 
   index_by(month=yearmonth(date)) %>% group_by(nature_of_incident) %>% 
   summarise(incidents=sum(incidents)) %>% 
   filter(nature_of_incident %in% selected_ni) %>% 
   gg_season()+
   facet_wrap(vars(nature_of_incident),ncol = 3, scales = "free_y")+
   labs(x="Month", y="Number of incident")

 #functions to investigate seasonality of nature of incidents
 incident_pattern_dayofweek <- function(ni) {
   incidents %>% 
     index_by(date) %>% group_by(nature_of_incident) %>% 
     summarise(incidents=sum(incidents)) %>% 
     filter(nature_of_incident == {{ ni }}) %>% 
     gg_season(period = "week") +
     labs(x="Day of Week", y="Number of incident", 
          title = glue::glue("Nature of incident:{ni}"))+
     theme(legend.position = "")
 }
 
 
 incident_pattern_month <- function(ni) {
   incidents %>% 
     index_by(month=yearmonth(date)) %>% group_by(nature_of_incident) %>% 
     summarise(incidents=sum(incidents)) %>% 
     filter(nature_of_incident == {{ ni }}) %>% 
     gg_season()+
     labs(x="Month", y="Number of incident", 
          title = glue::glue("Nature of incident: {ni}"))+
     theme(legend.position = "")
 }
 
 incident_pattern_week <- function(ni) {
   incidents %>% 
     index_by(week=yearweek(date)) %>% group_by(nature_of_incident) %>% 
     summarise(incidents=sum(incidents)) %>% 
     filter(nature_of_incident == {{ ni }}) %>% 
     gg_season(period = "year")+
     labs(x="Week", y="Number of incident", 
          title = glue::glue("Nature of incident: {ni}"))+
     theme(legend.position = "")
 }
 
 incident_pattern_dayofweek("BREATHING PROBLEMS")
 incident_pattern_month("BREATHING PROBLEMS")
 incident_pattern_week("BREATHING PROBLEMS")

# functions to investigate nature of incident & health board
 
 incident_pattern_dayofweek <- function(ni, hb) {
   incidents %>% filter(lhb_code == {{ hb }}) %>% 
     index_by(date) %>% group_by(nature_of_incident) %>% 
     summarise(incidents=sum(incidents)) %>% 
     filter(nature_of_incident == {{ ni }}) %>% 
     gg_season(period = "week") +
     labs(x="Day of Week", y="Number of incident", 
          title = glue::glue("Nature of incident:{ni}"))+
     theme(legend.position = "")
 }
 
 
 incident_pattern_month <- function(ni,hb) {
   incidents %>% filter(lhb_code == {{ hb }}) %>% 
     index_by(month=yearmonth(date)) %>% 
     group_by(nature_of_incident) %>% 
     summarise(incidents=sum(incidents)) %>% 
     filter(nature_of_incident == {{ ni }}) %>% 
     gg_season()+
     labs(x="Month", y="Number of incident", 
          title = glue::glue("Nature of incident: {ni}"))+
     theme(legend.position = "")
 }
 
 incident_pattern_week <- function(ni,hb) {
   incidents %>% filter(lhb_code == {{ hb }}) %>% 
     index_by(week=yearweek(date)) %>% group_by(nature_of_incident) %>% 
     summarise(incidents=sum(incidents)) %>% 
     filter(nature_of_incident == {{ ni }}) %>% 
     gg_season(period = "year")+
     labs(x="Week", y="Number of incident", 
          title = glue::glue("Nature of incident: {ni}"))+
     theme(legend.position = "")
 }
 
 incident_pattern_dayofweek("FALLS", "AB")
 incident_pattern_month("FALLS", "AB")
 incident_pattern_week("FALLS","AB")
 
 incidents %>% filter(lhb_code == "CV") %>% 
   index_by(date) %>% group_by(nature_of_incident) %>% 
   summarise(incidents=sum(incidents)) %>% 
   filter(nature_of_incident %in% selected_ni) %>% 
   gg_season(period = "week") +
   facet_wrap(vars(nature_of_incident),ncol = 3, scales = "free_y")+
   theme(legend.position = "")
 
# stl
 
 stl_fit <- incidents %>%
   model(
     stl = STL(incidents)
   ) %>%
   components()
 
 stl_incidents <- left_join(stl_fit, incidents) 
 
 stl_incidents %>%
   filter(nature_of_incident == "CARDIAC/RESPIRATORY ARREST/DEATH", lhb_code == "CV") %>% 
   ggplot(aes(x=max_air_temp, remainder)) +
   geom_point() +
   geom_smooth()
 
 holiday_rugby %>% 
   mutate(remainder =0, label = as.character(date)) %>% 
 filter(is_rugby ==1)->data_label
 
 holiday_rugby %>% 
   mutate(remainder =0, label = as.character(date)) %>% 
   filter(!is.na(public_holiday))->data_label
 

ggplot(data = stl_incidents, mapping = aes(x=date, y=remainder)) +
geom_line() +
geom_label_repel(data = data_label, aes(label =label))

ggplot(data = stl_incidents, mapping = aes(x=date, y=remainder)) +
  geom_line() +
  geom_label_repel(data = stl_incidents %>% 
                     filter(remainder > 11 | remainder < -11), 
                   aes(label = as.character(date)))
stl_incidents %>% 
  filter(remainder > 11 | remainder < -11) %>% pull(date)->extreme_values

holiday_rugby %>% select(public_holiday) %>% 
filter(!is.na(public_holiday)) %>% pull(date)->public_holiday_dates
intersect(public_holiday_dates,extreme_values)

library(ggridges)
incidents %>% group_by(lhb_code,nature_of_incident) %>% 
  index_by(date) %>% summarise(incidents = sum(incidents)) %>% 
  mutate(year =year(date)) %>% 
  ggplot(aes(x = incidents, y = as.factor(year), fill = lhb_code)) +
  geom_density_ridges(bandwidth = 1, alpha = 0.5) +
  facet_wrap(vars(factor(nature_of_incident)), nrow = 5, scales = "free") +
  #theme_hc() +
  theme(legend.position = "bottom") +
  labs(x = "Incident", y = "", fill = "Health Board",
       title = "Incidents by Nature & Healthboard - over the years")




# forecasting experiment
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
incidents %>% slice_max(n=5, order_by = incidents)
# prepare data, look at two different series 
breathing_problem_ts <- incidents %>% index_by(date) %>% 
  group_by(nature_of_incident) %>% 
  summarise(incidents = sum(incidents)) %>% 
  filter(nature_of_incident == "BREATHING PROBLEMS") %>%  
  dplyr::select(-nature_of_incident) %>% ungroup()

# this ts is intermittent
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
    TSCOUNT = fable.tscount::TSCOUNT(incidents ~ trend()+season("week") + fourier("year", 10), link = "log", model=list(past_obs=1:4))
  )

#log(x+1) or sqrt()
fit %>% dplyr::select(TSCOUNT) %>% gg_tsresiduals()
#model(fable.tscount::TSCOUNT(incidents ~ fourier("week", 3)))
fcst <- fit %>% forecast(times = 5000,  h = f_horizon, bootstrap = TRUE) 
fcst_sim <- fit %>%
  generate(times = 5000, h = f_horizon, bootstrap = TRUE) 
fcst_sim %>% filter(.sim <0)
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
fcst %>% group_by(.model) %>% 
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


#check .full = in  fill_gaps(incidents = 0, .full = TRUE)
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

# incidents %>% 
#   filter(lhb_code =="SB" , nature_of_incident == "HEART PROBLEMS/A.I.C.D",category == "RED")
