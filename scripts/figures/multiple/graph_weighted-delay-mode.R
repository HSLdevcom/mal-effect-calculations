# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(area == "Helsingin seutu") %>%
  dplyr::select(scenario, weighted_delay_car_all, weighted_delay_truck_all, weighted_delay_transit) %>%
  tidyr::pivot_longer(starts_with("weighted"), names_to = "mode", values_to = "value") %>%
  dplyr::mutate(
    mode = factor(mode,
                  levels = c("weighted_delay_car_all", "weighted_delay_truck_all", "weighted_delay_transit"),
                  labels = c("Henkilö- ja pakettiautot", "Kuorma-autot", "Joukkoliikennematkustajat")))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = mode, y = value)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1)(value)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1),
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
    y = "ekvivalentti-h / vrk"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_weighted-delay-mode.png"))
