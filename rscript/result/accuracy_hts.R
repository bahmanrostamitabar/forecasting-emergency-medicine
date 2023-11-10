# RMSSE <- function (.error, .train, na.rm = TRUE)
# {
#   .train <- diff(.train, lag = 7)
#   scale <- mean(.train^2, na.rm = na.rm)
#   sqrt(mean(.error^2/scale, na.rm = na.rm))
# }

library(tidyverse)
library(hts)
# hts_result_ets <- read_rds("result/hts_result_ets.rds")
# hts_result_ets <- read_rds("result/hts_result_arima.rds")
hts_result_ets <- read_rds("result/hts_result_tscount.rds")


#-----forecast------
fct <- hts_result_ets$fct_all
id <- tibble(.id = rep(1, 70))
a1 <- (fct[[1]]$forecast_all_level) %>% bind_cols(id)
a1[, ] <- NA
for (i in (1:nrow(hts_result_ets))) {
  a <- (fct[[i]]$forecast_all_level)
  id <- tibble(.id = rep(i, 70))
  a <- a %>% bind_cols(id)
  a1 <- a1 %>% bind_rows(a)
}
forecast_hts <- a1 %>% drop_na()
head(forecast_hts[, c(1, 1225:1226)])
write_rds(forecast_hts, "result/forecast/forecast_tscount.rds")

#----error---------

fct <- hts_result_ets$fct_all
id <- tibble(.id = rep(1, 70))
a1 <- (fct[[1]]$error_all_level) %>% bind_cols(id)
a1[, ] <- NA
for (i in (1:nrow(hts_result_ets))) {
  a <- (fct[[i]]$error_all_level)
  id <- tibble(.id = rep(i, 70))
  a <- a %>% bind_cols(id)
  a1 <- a1 %>% bind_rows(a)
}
error <- a1 %>% drop_na()

write_rds(error, "result/error/error_tscount.rds")

#---test set-------
# id <- tibble(.id=rep(1,70))
# a1 <- (fct[[1]]$testset_all_level) %>% bind_cols(id)
# a1[,] <- NA
# for (i in (1:nrow(hts_result_ets))) {
#   a <- (fct[[i]]$testset_all_level)
#   id <- tibble(.id=rep(i,70))
#   a <- a %>% bind_cols(id)
#   a1 <- a1 %>% bind_rows(a)
# }
# testset <- a1 %>% drop_na()
# write_rds(testset,"result/test/test.rds")
#----RMSSE----------
fct1 <- hts_result_ets$data_train

aa <- (fct1[[1]])
aaa <- aa[, 5:length(aa)]
gts_incident_train <- hts::gts(aaa,
  characters = list(c(1, 2), 3, 9)
)
agg <- aggts(gts_incident_train)
.train <- diff(agg, lag = 7)
scale <- colMeans(.train^2)

scale_m <- matrix(scale, nrow(error_m), nrow(error_m), byrow = TRUE)

s <- as_tibble(t(scale))

s[, ] <- NA
st <- s[, ]

for (i in (1:nrow(hts_result_ets))) {
  aa <- (fct1[[i]])
  aaa <- aa[, 5:length(aa)]
  gts_incident_train <- hts::gts(aaa,
    characters = list(c(1, 2), 3, 9)
  )
  agg <- aggts(gts_incident_train)
  .train <- diff(agg, lag = 7)
  scale <- colMeans(.train^2)
  s <- as_tibble(t(scale))
  ss <- s %>% slice(rep(1:n(), each = 70))
  st <- st %>% bind_rows(ss)
}
scale <- st %>% drop_na()

approach_id <- error %>% select(approach, .id)

scale_id <- scale %>% bind_cols(approach_id)


error_m <- error %>%
  select(-approach, -.id) %>%
  as.matrix()
scale_m <- scale %>% as.matrix()


error2_scale <- (error_m^2) / scale_m
ets_hts_rmsse <- as_tibble(error2_scale) %>% bind_cols(approach_id)

ets_hts_rmsse_long <- ets_hts_rmsse %>% pivot_longer(cols = 1:(ncol(ets_hts_rmsse) - 2), names_to = "level", values_to = "error2_scale")

ets_hts_rmsse_long_h <- ets_hts_rmsse_long %>%
  select(level, .id, approach, error2_scale) %>%
  group_by(level, .id, approach) %>%
  mutate(h = row_number())

write_rds(ets_hts_rmsse_long_h, "result/tscount_hts_rmsse_long_h.rds")

#-----MASE------

fct1 <- hts_result_ets$data_train

aa <- (fct1[[1]])
aaa <- aa[, 5:length(aa)]
gts_incident_train <- hts::gts(aaa,
  characters = list(c(1, 2), 3, 9)
)
agg <- aggts(gts_incident_train)
.train <- diff(agg, lag = 7)
scale <- colMeans(abs(.train))

scale_m <- matrix(scale, nrow(error_m), nrow(error_m), byrow = TRUE)

s <- as_tibble(t(scale))

s[, ] <- NA
st <- s[, ]

for (i in (1:nrow(hts_result_ets))) {
  aa <- (fct1[[i]])
  aaa <- aa[, 5:length(aa)]
  gts_incident_train <- hts::gts(aaa,
    characters = list(c(1, 2), 3, 9)
  )
  agg <- aggts(gts_incident_train)
  .train <- diff(agg, lag = 7)
  scale <- colMeans(abs(.train))
  s <- as_tibble(t(scale))
  ss <- s %>% slice(rep(1:n(), each = 70))
  st <- st %>% bind_rows(ss)
}
scale <- st %>% drop_na()

approach_id <- error %>% select(approach, .id)

scale_id <- scale %>% bind_cols(approach_id)


error_m <- error %>%
  select(-approach, -.id) %>%
  as.matrix()
scale_m <- scale %>% as.matrix()


error_scale <- (error_m) / scale_m
ets_hts_rmsse <- as_tibble(error_scale) %>% bind_cols(approach_id)

ets_hts_rmsse_long <- ets_hts_rmsse %>% pivot_longer(cols = 1:(ncol(ets_hts_rmsse) - 2), names_to = "level", values_to = "error_scale")

ets_hts_rmsse_long_h <- ets_hts_rmsse_long %>%
  select(level, .id, approach, error_scale) %>%
  group_by(level, .id, approach) %>%
  mutate(h = row_number())

write_rds(ets_hts_rmsse_long_h, "result/tscount_hts_mase_long_h.rds")
#---------

ets_hts_rmsse_long_h %>%
  filter(approach == "ets_comb", .id == 1 | .id == 2, level == "Total") %>%
  View()

rmss_hts <- ets_hts_rmsse_long %>%
  group_by(approach, .id, level) %>%
  summarise(rmsse = sqrt(mean(error2_scale)))

rm <- rmss_hts %>% filter(!is.nan(rmsse))

rm %>%
  group_by(approach) %>%
  summarise(rmsse_mean = mean(rmsse), rmsse_median = median(rmsse))

ggplot(data = rm %>% filter(level == "Total")) +
  geom_boxplot(aes(rmsse, fill = approach))
