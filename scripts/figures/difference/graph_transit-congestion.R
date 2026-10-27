# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario_ids <- c("2023", "2045_ve0", "2045_tavoiteverkko", "2045_suunnitelma")

results <- purrr::map_dfr(scenario_ids, function(scenario_id) {
  scenario <- scenarios %>%
    dplyr::filter(scenario == scenario_id)

  if (nrow(scenario) != 1) {
    rlang::abort(sprintf("Expected exactly one scenario definition for %s.", scenario_id))
  }

  data <- readr::read_delim(
    here::here("results", scenario$results, "transit_congestion.csv"),
    delim = ";",
    col_types = readr::cols(
      congestion_max_aht = readr::col_double(),
      total_capacity_aht = readr::col_double(),
      .default = readr::col_skip()
    )
  )

  valid_data <- data %>%
    dplyr::filter(
      is.finite(congestion_max_aht),
      is.finite(total_capacity_aht),
      total_capacity_aht > 0
    )

  total_capacity <- sum(valid_data$total_capacity_aht)
  if (nrow(valid_data) == 0 || !is.finite(total_capacity) || total_capacity <= 0) {
    rlang::abort(sprintf("No valid AHT congestion and capacity data for scenario %s.", scenario_id))
  }

  tibble::tibble(
    scenario = sprintf("%d %s", scenario$year, scenario$name),
    weighted_congestion = stats::weighted.mean(
      valid_data$congestion_max_aht,
      valid_data$total_capacity_aht
    )
  )
}) %>%
  dplyr::mutate(scenario = factor(scenario, levels = scenario))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = weighted_congestion, fill = scenario)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 0.1, decimal.mark = ",")(weighted_congestion)),
    vjust = -0.5,
    size = points2mm(8)
  ) +
  scale_fill_manual(values = hsl_blues_fill) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Linjojen maksimikuormitus",
    x = NULL,
    y = "Painotettu keskiarvo"
  ) +
  theme_mal_graph() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave_graph(here::here("figures", "graph_transit-congestion.png"))
