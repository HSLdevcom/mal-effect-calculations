# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(area %in% "Helsingin seutu") %>%
  dplyr::select(scenario, starts_with("co2")) %>%
  tidyr::pivot_longer(
    cols = starts_with("co2"),
    names_to = "vehicle",
    names_prefix = "co2_",
  ) %>%
  dplyr::mutate(
    vehicle = factor(vehicle, levels = c("truck_all", "bus_other", "bus_hsl", "van", "car"), labels = c("Kuorma-autot", "Muu linja-autoliikenne", "HSL:n linja-autoliikenne", "Pakettiautot", "Henkilöautot"))
  )

results_total <- results %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(value = sum(value))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = value)) +
  geom_col(aes(fill = vehicle), position = position_stack()) +
  geom_text(
    aes(label = scales::label_number(scale = 0.000001, accuracy = 1)(value), group = vehicle),
    position = position_stack(vjust = 0.5),
    size = points2mm(8)
  ) +
  geom_text(data = results_total,
            aes(label = scales::label_number(scale = 0.000001, accuracy = 1)(value)),
            vjust = -0.5,
            size = points2mm(8),
            fontface = "bold") +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.000001)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#333333", "#AAD3ED", "#007AC9", "#F7C8E6", "#f092cd")
  ) +
  labs(
    title = "Tieliikenteen CO2-päästöt",
    x =  NULL,
    y = "tonnia CO2 ekv. / arki-vrk"
  ) +
  theme_mal_graph() +
  theme(legend.position = "right")

ggsave_graph(here::here("figures", "graph_co2.png"))
