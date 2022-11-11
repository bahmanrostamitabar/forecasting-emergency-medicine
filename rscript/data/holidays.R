# Script to turn original data file of holidays into an RDS for later use
# Input: holiday_rugby.xlsx
# Output: holiday_dummy.rds

library(tidyverse)
library(lubridate)
library(tsibble)

# read holiday data
holiday <- readxl::read_excel(here::here("data/holiday_rugby.xlsx")) |>
  select(date, public_holiday, school_holiday) |>
  mutate(date = as_date(date)) |>
  mutate(
    public_holiday_d = if_else(is.na(public_holiday), 0, 1),
    school_holiday_d = if_else(is.na(school_holiday), 0, 1)
  ) |>
  mutate(
    xmas = if_else(public_holiday == "Christmas Day", "1", "0"),
    new_years_day = if_else(public_holiday == "New Years Day", 1, 0)
  ) |>
  mutate_at(vars(xmas, new_years_day), ~ replace(., is.na(.), 0))

# create dummies for holidays
holiday_dummy <- holiday |>
  select(
    date,
    public_holiday_d,
    school_holiday_d,
    xmas,
    new_years_day
  ) |>
  as_tsibble(index = date)

# Save as RDS for reading in by other scripts
write_rds(holiday_dummy, paste0(storage_folder, "holiday_dummy.rds"), compress="bz2")


# Other holiday tibbles created below, no longer used (?)

holiday_school <- holiday_dummy |>
  select(date, school_holiday_d) |>
  filter(school_holiday_d == 1) |>
  mutate(holiday = "school_holiday") |>
  select(-school_holiday_d)
holiday_public <- holiday_dummy |>
  select(date, public_holiday_d) |>
  filter(public_holiday_d == 1) |>
  mutate(holiday = "public_holiday") |>
  select(-public_holiday_d)
holiday_xmas <- holiday_dummy |>
  select(date, xmas) |>
  filter(xmas == 1) |>
  mutate(holiday = "xmas") |>
  select(-xmas)
holiday_newyear <- holiday_dummy |>
  select(date, new_years_day) |>
  filter(new_years_day == 1) |>
  mutate(holiday = "new_years_day") |>
  select(-new_years_day)
school_holiday <- tibble(
  holiday = "school_holiday",
  ds = holiday_school$date,
  lower_window = -1,
  upper_window = 1
)
public_holiday <- tibble(
  holiday = "public_holiday",
  ds = holiday_public$date,
  lower_window = -1,
  upper_window = 1
)
xmas_holiday <- tibble(
  holiday = "xmas",
  ds = holiday_xmas$date,
  lower_window = -1,
  upper_window = 1
)
new_years_day_holiday <- tibble(
  holiday = "new_years_day",
  ds = holiday_newyear$date,
  lower_window = -1,
  upper_window = 1
)
holiday_prophet <- school_holiday |>
  bind_rows(public_holiday) |>
  bind_rows(xmas_holiday) |>
  bind_rows(new_years_day_holiday)
