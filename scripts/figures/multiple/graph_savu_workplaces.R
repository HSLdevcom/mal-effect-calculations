# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = goodness_share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = goodness_share_lower, ymax = goodness_share_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = goodness_share / 2,
      label = scales::label_percent(accuracy = 1, suffix = "")(goodness_share),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = " %"),
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
    title = "Työpaikkojen sijoittuminen kestävän liikkumisen kannalta hyville\nsaavutettavuusvyöhykkeille",
    x = NULL,
    y = NULL
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_savu_workplaces.png"))
