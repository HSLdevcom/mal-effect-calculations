# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(area == "Helsingin seutu") %>%
  dplyr::select(scenario, starts_with(c("weighted_delay_car_all", "weighted_delay_truck_all", "weighted_delay_transit"))) %>%
  tidyr::pivot_longer(!c(scenario, ends_with(c("lower", "upper"))), names_to = "mode", values_to = "value") %>%
  dplyr::mutate(
    lower = dplyr::case_when(
      mode == "weighted_delay_car_all" ~ weighted_delay_car_all_lower,
      mode == "weighted_delay_truck_all" ~ weighted_delay_truck_all_lower,
      mode == "weighted_delay_transit" ~ weighted_delay_transit_lower,
      TRUE ~ NA_real_
    ),
    upper = dplyr::case_when(
      mode == "weighted_delay_car_all" ~ weighted_delay_car_all_upper,
      mode == "weighted_delay_truck_all" ~ weighted_delay_truck_all_upper,
      mode == "weighted_delay_transit" ~ weighted_delay_transit_upper,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(scenario, mode, value, lower, upper) %>%
  dplyr::mutate(
    mode = factor(mode,
                  levels = c("weighted_delay_car_all", "weighted_delay_truck_all", "weighted_delay_transit"),
                  labels = c("Henkilö- ja pakettiautot", "Kuorma-autot", "Joukkoliikennematkustajat")))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = mode, y = value)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = lower, ymax = upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(y = value / 2, label = scales::label_number(accuracy = 1, scale = 0.001)(value)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1, scale = 0.001),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(20)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#007AC9", "#54A5DA", "#AAD3ED")
  ) +
  labs(
    title = "Tieliikenteen ruuhkautuvuussuorite kulkutavoittain",
    x = NULL,
    y = "tuhatta ekvivalentti-h / vrk"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_weighted-delay-mode.png"))
