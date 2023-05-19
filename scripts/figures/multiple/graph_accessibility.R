# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = accessibility_scaled)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = accessibility_scaled_lower, ymax = accessibility_scaled_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = accessibility_scaled / 2,
      label = scales::label_number(accuracy = 1)(accessibility_scaled),
      color = scenario
    ),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_number(),
    limits = c(0, 100),
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
    title = "Saavutettavuus asukkaiden näkökulmasta",
    x = NULL,
    y = "indeksi"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_accessibility.png"))
