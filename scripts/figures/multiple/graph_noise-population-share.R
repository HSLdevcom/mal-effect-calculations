# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::filter(area != "Pääkaupunkiseutu")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = noise_population_share, fill = scenario, group = scenario)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(
    mapping = aes(ymin = noise_population_share_lower, ymax = noise_population_share_upper),
    position =  position_dodge(width = 0.9),
    width = 0.2,
    na.rm = TRUE,
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = noise_population_share / 2,
      label = scales::label_percent(accuracy = 1, suffix = "")(noise_population_share),
      color = scenario
    ),
    position = position_dodge(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = ""),
    limits = c(0, 1)
  ) +
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 18)
  ) +
  scale_fill_manual(
    name = NULL,
    values = mal_greens_fill
  ) +
  scale_color_manual(
    guide = "none",
    values = mal_color
  ) +
  labs(
    title = "Meluvyöhykkeillä asuvien asukkaiden osuus alueen asukkaista",
    subtitle = "Melutaso vähintään 55 dB",
    x =  NULL,
    y = "%"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_mal_graph() +
  theme(axis.text.x = element_text(size = 8))

ggsave_graph(here::here("figures", "graph_noise-population-share.png"), width = 240, height = 120)
