# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::mutate(noise_population_share = noise_population / total_pop,
                noise_population_share_lower = noise_population_lower / total_pop,
                noise_population_share_upper = noise_population_upper / total_pop)


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = noise_population_share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = noise_population_share_lower, ymax = noise_population_share_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(
      y = noise_population_share / 2,
      label = scales::label_percent(accuracy = 1, suffix = "")(noise_population_share),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = ""),
    limits = c(0, 1)
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
    title = "Meluvyöhykkeillä asuvien asukkaiden osuus alueen asukkaista",
    x =  NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_noise-population-share.png"), width = 150, height = 84)
