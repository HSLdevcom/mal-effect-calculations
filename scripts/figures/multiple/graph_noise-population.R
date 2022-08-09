# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = noise_population, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = noise_population_lower, ymax = noise_population_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(
      y = noise_population / 2,
      label = scales::label_number(scale = 0.001, accuracy = 1)(noise_population),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    name = "tuhatta asukasta",
    labels = scales::label_number(scale = 0.001),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = mal_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  labs(
    title = "Meluvyöhykkeillä asuvien asukkaiden määrä",
    x =  NULL,
    y = NULL
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_noise-population.png"))
