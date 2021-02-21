library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(readxl)
library(fable)
library(feasts)
library(janitor)
library(ggthemes)

# preparation------------------------------------------
wast_data <- read_excel("data/Nature_of_Incidents_Attended.xlsx")
incidents <- wast_data %>% 
  mutate(Incident_Date=as_date(Incident_Date)) %>% 
  janitor::clean_names() %>% 
  force_tz(incident_date, tz="GB") %>% 
  select(-lhb_name,-nature_of_incident, -nature_of_incident_description)

incidents <- incidents %>% 
count(lhb_code, category, mpds_priority,incident_date, name = "incidents") %>% 
  as_tsibble(index = incident_date, key = c(lhb_code, category, mpds_priority)) %>% 
  fill_gaps(incidents=0)

incidents_gts <- incidents %>%
  aggregate_key(mpds_priority * category * lhb_code, incidents = sum(incidents))

# plot ---------------------------------------------

library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))

library(dygraphs)
incidents %>% group_by(category) %>% summarise(incidents=sum(incidents))->wast_cat
tsbox::ts_xts(wast_cat) %>% 
  dygraph() %>% 
  dyRangeSelector(dateWindow = c("2015-10-01","2015-10-01"))

incidents %>% group_by(lhb_code) %>% summarise(incidents=sum(incidents))->wast_cat
tsbox::ts_xts(wast_cat) %>% 
  dygraph() %>% 
  dyRangeSelector(dateWindow = c("2015-10-01","2015-10-01"))

incidents_gts_feature <- incidents_gts %>% 
  features(incidents, feature_set(pkgs = "feasts"))

# is there any seasonality/trend?
incidents_gts_feature %>% 
  ggplot(aes(x = trend_strength, y = seasonal_strength_week)) +
  geom_point()

# How easy is the ts to forecast?
incidents_gts_feature %>% 
  ggplot(aes(x = spectral_entropy, y = coef_hurst)) +
  geom_point()

# Plotting different levels
p1 <- incidents_gts %>%
  filter(!is_aggregated(mpds_priority), is_aggregated(category), is_aggregated(lhb_code)) %>%
  autoplot(incidents)
p1

p2 <- incidents_gts %>%
  filter(is_aggregated(mpds_priority), !is_aggregated(category), is_aggregated(lhb_code)) %>%
  autoplot(incidents)
p2

p3 <- incidents_gts %>%
  filter(is_aggregated(mpds_priority), is_aggregated(category), !is_aggregated(lhb_code)) %>%
  autoplot(incidents)

p3

p4 <- incidents_gts %>%
  filter(is_aggregated(mpds_priority), is_aggregated(category), is_aggregated(lhb_code)) %>%
  autoplot(incidents)


p4


p <- incidents %>%
  features(incidents, feat_stl) %>% 
  ggplot(aes(x = trend_strength, y = seasonal_strength_week, 
             col = mpds_priority)) +
  geom_point() +
  facet_wrap(vars(lhb_code))+
  scale_color_colorblind()
p

# Check trend/seasonality at bottom level

p <- incidents %>% group_by(lhb_code,category) %>% 
  summarise(incidents = sum(incidents)) %>% 
  features(incidents, feat_stl) %>% 
  ggplot(aes(x = trend_strength, y = seasonal_strength_week, 
             col = category)) +
  geom_point() +
  facet_wrap(vars(lhb_code))+
  scale_color_colorblind()
p

# How noisy are the bottom series?

ggplot(incidents, mapping = aes(x=incident_date, y=incidents))+
  geom_line(aes(colour=category ))+
  facet_grid(rows=vars(lhb_code), cols =  vars(mpds_priority), scales = "free_y")+
  scale_colour_manual(
    name = "Category",values=c("#FFBF00","#008000", "#FF0000")
  )

incidents %>% 
  group_by(lhb_code,category) %>% 
  summarise(incidents = sum(incidents)) %>% 
ggplot(mapping = aes(x=incident_date, y=incidents))+
  geom_line(aes(colour=category ))+
  facet_grid(rows=vars(lhb_code), scales = "free_y")+
  scale_colour_manual(
    name = "Category",values=c("#FFBF00","#008000", "#FF0000")
  )

# how strong is the seasonality for each series at health board, category
season_plot <- function(hb,cat) {
incidents %>% group_by(lhb_code,category) %>% 
  summarise(incidents = sum(incidents)) %>%
  filter(category == cat, lhb_code == hb) %>% 
  gg_season(incidents, period = "week") + 
  theme(legend.position = "")
}

subseries_plot <- function(hb,cat) {
  incidents %>% group_by(lhb_code,category) %>% 
    summarise(incidents = sum(incidents)) %>%
    filter(category == cat, lhb_code == hb) %>% 
    gg_subseries(incidents, period = "week") + 
    theme(legend.position = "")
}

#hb == "AB"  "BC"  "CTM" "CV"  "HD"  "POW" "SB" 
#cat == "AMBER" "GREEN" "RED"  
season_plot("CV", "GREEN")
subseries_plot("POW", "RED")

#--Autocorrelation-------------------------------------------------------

vis_acf <- function(hb, cat) {
  incidents %>% filter(category == cat, lhb_code == hb) ->dayily_out
  ggAcf(dayily_out$incidents, lag.max = 21)
}
vis_acf("POW","RED")

#Forecasting--------------------------

#to do:
# time series cross validation
# add more models!
# report accuracy in all levels
# check lower quantile for potential negative values in bottom level

f_horizon <- 7
train <- incidents_gts %>% filter_index(~  "2019-07-23")

last(incidents_gts$incident_date)-years(1)
last(incidents_gts$incident_date)-days(f_horizon)

train <- incidents_gts %>% filter_index(. ~ "2018-07-31")
(train$incident_date) %>% unique() %>% length()

ae_tscv <- incidents_gts %>% filter_index(. ~ "2019-07-24") %>% 
  stretch_tsibble(.init = 1035, .step = 7)

ae_test <- incidents_gts %>% 
  filter_index( "2018-08-01" ~ .) %>% 
  slide_tsibble(.size = f_horizon, .step = 7)

fit_incident <- train %>%
  model(base = ETS(incidents ~ error("A") + trend("A") + season("N"))) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )

fcst_incident <- fit_incident %>% forecast(h=f_horizon)

fcst_incident %>% accuracy(incidents_gts,  
                           measures = list(rmse = RMSE, mae = MAE)) %>% 
  group_by(.model) %>% summarise(rmse = mean(rmse), mae = mean(mae))

