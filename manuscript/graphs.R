library(tidyverse)

options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.color = c("#D55E00", "#0072B2", "#CC79A7", "#E69F00", "#009E73", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#CC79A7", "#E69F00", "#009E73", "#56B4E9", "#F0E442")
)

rmsse <- readr::read_rds(here::here("results/rmsse.rds"))
mase <- readr::read_rds(here::here("results/mase.rds"))
crps <- readr::read_rds(here::here("results/crps.rds")) |>
  pivot_wider(names_from = model, values_from = crps) |>
  group_by(method, h, series) |>
  mutate(across(where(is.numeric), ~ .x / naiveecdf)) |>
  ungroup() |>
  pivot_longer(ensemble:tscount, names_to = "model", values_to = "crps")

accuracy <- bind_rows(
  rmsse |> mutate(measure = "msse", accuracy = rmsse^2),
  mase |> mutate(measure = "mase", accuracy = mase),
  crps |> mutate(measure = "crps", accuracy = crps),
) |>
  select(-rmsse, -mase, -crps)

# Plot of average accuracy vs week for each method for Total
acc_summary <- accuracy |>
  filter(
    series %in% c("Total", "Control areas", "Health boards"),
    method == "mint", model != "qcomb"
  ) |>
  mutate(
    series = factor(series, levels = c("Total", "Control areas", "Health boards")),
    model = factor(model, levels = c("naiveecdf", "ets", "tscount", "iglm", "ensemble")),
    week = factor(trunc((h - 1) / 7) + 1)
  ) |>
  group_by(week, model, measure, series) |>
  summarise(accuracy = mean(accuracy), .groups = "drop")

acc_summary |>
  ggplot(aes(x = week, y = accuracy, group = model)) +
  geom_line(aes(col = model), size = 1) +
  facet_grid(measure ~ series, scales = "free_y") +
  labs(y = "Average accuracy", x = "Week ahead")
