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

incidents <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") %>%
  select(-nature_of_incident, -nature_of_incident_description) %>%
  count(lhb_code, lhb_name, category, mpds_priority, date, name = "incidents") %>%
  mutate(
    category = factor(category, level=c("RED","AMBER","GREEN")),
    mpds_priority = factor(mpds_priority, level=c("RED","AMBER1","AMBER2","GREEN2","GREEN3"))
  ) %>%
  left_join(temperatures, by="date") %>%
  as_tsibble(index = date, key = c(lhb_name, category, mpds_priority)) %>%
  fill_gaps(incidents = 0)

incidents_gts <- incidents %>%
  aggregate_key(category/mpds_priority * lhb_name, incidents = sum(incidents))

# Colors for plots
colors <- c(`<aggregated>` = "#000000",
            RED = "#ff0000", 
            AMBER = "#ff9900", AMBER1 = "#ff9900", AMBER2 = "#ffdd00", 
            GREEN = "#009900", GREEN2 = "#009900", GREEN3 = "#00ff00")
lhb_colors <- c("#56b4e9","#0072b2","#009e73","#f0e442","#d55e00","#e69f00","#cc79a7")
names(lhb_colors) <- sort(unique(incidents$lhb_name))

# Plot sum of incidents at different levels

incidents_gts %>%
  filter(is_aggregated(mpds_priority), is_aggregated(category), is_aggregated(lhb_name)) %>%
  autoplot(incidents)

incidents_gts %>%
  filter(is_aggregated(mpds_priority), !is_aggregated(category), is_aggregated(lhb_name)) %>%
  select(date, category, incidents) %>%
  mutate(category = factor(category, level=c("RED","AMBER","GREEN"))) %>%
  autoplot(incidents) +
  scale_color_manual(values=colors)

incidents_gts %>%
  filter(is_aggregated(mpds_priority), is_aggregated(category), !is_aggregated(lhb_name)) %>%
  select(date, lhb_name, incidents) %>%
  autoplot(incidents) +
  scale_color_manual(values = lhb_colors)

# Compute features ----------------------------------------------------------

incidents_gts_feature <- incidents_gts %>%
  features(incidents, feature_set(pkgs = "feasts")) %>%
  mutate(
    category = factor(as.character(category), level=c("<aggregated>", "RED","AMBER","GREEN")),
    lhb_name = factor(as.character(category), level=c("<aggregated>", sort(unique(incidents$lhb_name))))
  )
  
# is there any seasonality/trend?

incidents_gts_feature %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_week, col=factor(category))) +
  geom_point()  +
  scale_color_manual(name = "Category", values = colors) +
  geom_label_repel(data = incidents_gts_feature %>% filter(seasonal_strength_week > 0.25),
                   aes(label = glue::glue("{lhb_name}/{category}/{mpds_priority}")),
                   show.legend=FALSE)

# How easy is the ts to forecast?

incidents_gts_feature %>%
  ggplot(aes(x = spectral_entropy, y = coef_hurst, col=factor(category))) +
  geom_point() +
  scale_color_manual(name = "Category", values = colors) +
  geom_label_repel(data = incidents_gts_feature %>% filter(spectral_entropy < 0.7),
                 aes(label = glue::glue("{lhb_name}/{category}/{mpds_priority}")),
                 show.legend = FALSE)

incidents %>%
  features(incidents, feat_stl) %>%
  ggplot(aes(
    x = trend_strength, y = seasonal_strength_week,
    col = mpds_priority
  )) +
  geom_point() +
  facet_wrap(vars(lhb_name)) +
  scale_color_manual(name = "MPDS priority", values=colors)

# Check trend/seasonality at bottom level

incidents %>%
  group_by(lhb_name, category) %>%
  summarise(incidents = sum(incidents)) %>%
  features(incidents, feat_stl) %>%
  ggplot(aes(
    x = trend_strength, y = seasonal_strength_week,
    col = category
  )) +
  geom_point() +
  facet_wrap(vars(lhb_name)) +
  scale_color_manual(name = "Category", values=colors)

# How noisy are the bottom series?

incidents %>%
  ggplot(mapping = aes(x = date, y = incidents)) +
  geom_line(aes(colour = mpds_priority)) +
  facet_grid(rows = vars(lhb_name), cols = vars(mpds_priority), scales = "free_y") +
  scale_color_manual(values=colors) +
  guides(col=FALSE)

# how strong is the seasonality for each series at health board, category
season_plot <- function(hb, cat) {
  incidents %>%
    group_by(lhb_code, category) %>%
    summarise(incidents = sum(incidents)) %>%
    filter(category == cat, lhb_code == hb) %>%
    fill_gaps(incidents=0) %>%
    gg_season(incidents, period = "week") +
    theme(legend.position = "")
}

subseries_plot <- function(hb, cat) {
  incidents %>%
    group_by(lhb_code, category) %>%
    summarise(incidents = sum(incidents)) %>%
    filter(category == cat, lhb_code == hb) %>%
    fill_gaps(incidents=0) %>%
    gg_subseries(incidents, period = "week") +
    theme(legend.position = "")
}

# hb == "AB"  "BC"  "CTM" "CV"  "HD"  "POW" "SB"
# cat == "AMBER" "GREEN" "RED"
season_plot("POW", "RED")
subseries_plot("POW", "RED")

#--Autocorrelation-------------------------------------------------------

vis_acf <- function(hb, cat) {
  incidents %>% 
    filter(category == cat, lhb_code == hb) %>%
    fill_gaps(incidents=0) %>%
    ACF(incidents) %>%
    autoplot()
}
vis_acf("POW", "RED")

## Relationship with temperature

stl_fit <- incidents %>%
  model(
    stl = STL(incidents)
  ) %>%
  components()

left_join(stl_fit, incidents) %>%
  ggplot(aes(x=min_air_temp, remainder)) +
  geom_point() +
  geom_smooth()

# Hmm. No evidence of a relationship with temperature!

# Forecasting--------------------------

# to do:
# time series cross validation
# add more models!
# report accuracy in all levels
# check lower quantile for potential negative values in bottom level

f_horizon <- 28
train <- incidents_gts %>% filter(date < max(date) - f_horizon)

fit_incident <- train %>%
  model(base = ETS(incidents ~ error("A") + trend("A") + season("A"))) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )

fcst_incident <- fit_incident %>% forecast(h = f_horizon)

fcst_incident %>%
  filter(is_aggregated(category), is_aggregated(lhb_name)) %>%
  autoplot(incidents_gts %>% filter(year(date) >= 2019))

fcst_incident %>%
  accuracy(incidents_gts,
    measures = list(rmsse = RMSSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase))

# Look at MASE over several levels for MinT
fcst_mint <- fcst_incident %>%
  filter(.model=='mint') %>%
  accuracy(incidents_gts,
           measures = list(rmsse = RMSSE, mase = MASE)
  ) 
# Bottom level
fcst_mint %>% 
  filter(!is_aggregated(category), !is_aggregated(mpds_priority), !is_aggregated(lhb_name)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase))
# At LHB level
fcst_mint %>% 
  filter(is_aggregated(category), !is_aggregated(lhb_name)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase))
# At category level
fcst_mint %>% 
  filter(is_aggregated(lhb_name), is_aggregated(mpds_priority), !is_aggregated(category)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase))
# Top level
fcst_mint %>% 
  filter(is_aggregated(category), is_aggregated(lhb_name)) %>%
  summarise(rmsse = mean(rmsse), mase = mean(mase))

