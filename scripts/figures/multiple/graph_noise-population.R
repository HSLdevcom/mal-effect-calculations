# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(area != "Helsingin seutu")

results_total <- results %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(noise_population = sum(noise_population))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = noise_population)) +
  geom_col(aes(fill = area), position = position_stack()) +
  geom_text(
    aes(label = scales::label_number(scale = 1, accuracy = 1000)(noise_population), group = area),
    position = position_stack(vjust = 0.5),
    size = points2mm(8),
    color = "#333333"
  ) +
  geom_text(data = results_total,
            aes(label = scales::label_number(scale = 1, accuracy = 1000)(noise_population)),
            vjust = -0.5,
            size = points2mm(8),
            fontface = "bold",
            color = "#333333") +
  scale_y_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#BFD7AC", "#f092cd", "#007AC9", "#AAD3ED")
  ) +
  labs(
    title = "Meluvyöhykkeillä asuvien määrä",
    x =  NULL,
    y = NULL
  ) +
  theme_mal_graph() +
  theme(legend.position = "right")

ggsave_graph(here::here("figures", "graph_noise-population.png"))
