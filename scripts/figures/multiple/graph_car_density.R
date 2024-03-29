# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = car_density, fill = scenario)) +
  geom_col(position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = car_density_lower, ymax = car_density_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = car_density / 2,
      label = scales::label_number(accuracy = 1)(car_density),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
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
    title = "Henkilöautotiheys",
    x =  NULL,
    y = "autoa / 1000 asukasta"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_car_density.png"))
